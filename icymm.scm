;;; icymm.scm --- an irc bot for #emacs-cn@irc.debian.org

;; Copyright (C) 2008, 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.2a

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;;  Written in chicken scheme.

;;; Code:

(use posix tcp irc regex srfi-1 srfi-13 args srfi-18 
     http-client html-parser sxpath format json srfi-19)

;;; Global Variables
(define icymm-server "irc.debian.org")
(define icymm-port 6667)
(define icymm-fortune-file "icymm.fortune")
(define icymm-logging-p #t)

(define icymm-nick "icymm")
(define icymm-channel "#emacs-cn")
(define icymm-password #f)
(define icymm-real-name "湘琴")

(define icymm-ip-data "~/etc/QQWry.dat")

(define icymm-connection #f)
(define icymm-start-time #f)

(define icymm-irc-nick-regexp "[][^_{|}a-zA-Z0-9-][][^_{|}a-zA-Z0-9-]*")


;;; Data Table

(define (icymm-receiver-is-me? receiver)
  (equal? receiver (irc:connection-nick icymm-connection)))

;; '(receiver sender msg)
(define icymm-tell-table '())
(define icymm-aliases '())

(define icymm-tell-table-cache-file "~/.icymm-cache")

(define (icymm-cache-save)
  "Save `icymm-tell-table' to `icymm-tell-table-cache-file'."
  (with-output-to-file icymm-tell-table-cache-file
    (lambda ()
      (print
       (format
        ";; -*- scheme -*-

 (set! icymm-tell-table '~S)

 (set! icymm-aliases '~S)

 (set! icymm-seen-table '~S)"
        icymm-tell-table
        icymm-aliases
        icymm-seen-table
        )))))

(define (icymm-tell-table-load)
  "Load `icymm-tell-table' from `icymm-tell-table-cache-file'."
  (when (file-read-access? icymm-tell-table-cache-file)
    (load icymm-tell-table-cache-file)))

(define (icymm-tell-table-add! receiver sender msg)
  (set! icymm-tell-table
        (append icymm-tell-table (list (list receiver sender msg))))
  (icymm-cache-save))

(define (icymm-tell-table-remove! record)
  ;; Not alist-delete here, since a user may recieve multiple messages
  ;; from different people.
  (set! icymm-tell-table (delete record icymm-tell-table))
  (icymm-cache-save))

;;; IRC commands and functions

(define (icymm-response msg response)
;;   (display (format "(sender, receiver): (~A, ~A)\n"
;;                    (irc:message-sender msg)
;;                    (irc:message-receiver msg)))
;;   (display (format "body: ~A\n" (irc:message-body msg)))
  (if (icymm-receiver-is-me? (irc:message-receiver msg))
      (irc:say icymm-connection response (irc:message-sender msg))
      (irc:say icymm-connection response)))

(define (icymm-notice msg notice)
  (if (icymm-receiver-is-me? (irc:message-receiver msg))
      ;; (irc:notice icymm-connection notice (irc:message-sender msg))
      (irc:say icymm-connection notice (irc:message-sender msg))
    ;; (irc:notice icymm-connection notice)))
    (irc:say icymm-connection notice)))

(define (icymm-add-privmsg-handler! command callback tag)
  (irc:add-message-handler!
   icymm-connection
   callback
   command: "PRIVMSG"
   body: (lambda (msg)
           (string-search
            (regexp
             (format "PRIVMSG ~A :~A: ~A|PRIVMSG ~A :~A|PRIVMSG ~A :~A"
                     icymm-channel icymm-nick command
                     icymm-nick command
                     icymm-channel command))
            (irc:message-body msg)))
   tag: tag))

(define (icymm-wait regexp)
  "Wait until we received texts matching REGEXP."
  (or (string-search regexp (irc:message-body (irc:wait icymm-connection)))
      (icymm-wait regexp)))

(define (icymm-get-ip nick)
  (irc:command icymm-connection (string-append "whois " nick))
  (cadr (icymm-wait
         (string-append 
          nick
          " ([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}) :actually using host"
          ))))

(define (icymm-names)
  (irc:command icymm-connection (string-append "names " icymm-channel))
  (string-split
   (cadr 
    (icymm-wait
     (format "353.+~A = ~A :(.+)" icymm-nick icymm-channel)))))

(define (icymm-user-online? user)
  (member user (icymm-names)))

;;; Callbacks

(define (icymm-format-url url)
  "在 URL 前加一些前缀。"
 (string-append "没记错的话，它是：" url))

(define (icymm-help-callback msg)
  (icymm-response 
   msg
   "支持的命令：,help ,time ,tell ,uptime ,emms ,paste ,alias ,weather ,joke ,ip ,seen..."))

(define (icymm-default-callback msg)
  (letrec ((fortune-reader
            (lambda (s)
              (let ((tmp (read-line)))
                (if (eof-object? tmp)
                    s
                  (fortune-reader (string-append s tmp " ")))))))
    (let* ((s (with-input-from-pipe 
               (string-append "fortune || cat "  icymm-fortune-file)
               (lambda () (fortune-reader ""))))
           (s1 (string-split s "%"))
           (i (random (length s1))))
      (icymm-response 
       msg (string-trim-both (list-ref s1 i))))))

(define (icymm-tell-callback msg)
  "发离线消息。"
  (let* ((sender (irc:message-sender msg))
         (body (irc:message-body msg))
         (positions (string-search-positions
                     (regexp (format ",tell (~A) ?" icymm-irc-nick-regexp)) body)))
    (when positions
      (let* ((future-receiver (apply substring body (cadr positions)))
             (content (substring body (cadr (car positions))))
             (alias (icymm-alias-online? future-receiver)))
        ;; (print alias)
        ;; (newline)
        (if alias
            (icymm-response msg (format "笨笨，伊有别名 ~A 在线啊！" alias))
          (begin 
            (icymm-tell-table-add! 
             future-receiver 
             sender 
             (string-append "[" (icymm-tell-timestamp) "] " content))
            (icymm-response msg
                            (format "收到，下次 ~A 上线的时候，代为转告！"
                                    future-receiver))))))))

(define (icymm-alias-online? nick)
  "Check `icymm-aliases' to see whether NICK has any other aliases online."
  (let ((aliases (icymm-alias-find nick))
        (names (icymm-names)))
    ;; (display names) (newline)
    ;; (display nick) (newline)
    (cond ((member nick names)
           nick)
          (aliases
           (let loop ((ali aliases))
             ;; (print ali) (newline)
             (cond ((not (list? ali))
                    #f)
                   ((member (car ali) names)
                    ;; (print "..") (print (car ali)) (newline)
                    (car ali))
                   (else
                    (loop (cdr ali))))))
          (else
           #f))))

(define (icymm-alias-find nick)
  "Look up aliases for nick from `icymm-aliases'."
  (let loop ((ali icymm-aliases))
    (cond ((null? ali)
           #f)
          ((member nick (car ali))
           (car ali))
          (else
           (loop (cdr ali))))))

(define (icymm-join-callback msg)
  (let* ((body (irc:message-body msg))
         (positions (string-search-positions
                     (regexp (format ":(~A)!" icymm-irc-nick-regexp)) body)))
    ;; (unless positions (icymm-response msg "not matched?"))
    (when positions
          ;; (icymm-response msg "matched?")
          (let ((who (apply substring body (cadr positions)))
                (record #f))
            (let loop ()
              (set! record (assoc who icymm-tell-table))
              (when record
                    (icymm-response
                     msg
                     (format "~A: ~A 给你留言了：~S"
                             who (list-ref record 1) (list-ref record 2)))
                    (icymm-tell-table-remove! record)
                    (loop)))))))

(define (icymm-你好-callback msg)
  (icymm-response msg "好，好，大家好！"))

(define (icymm-dirty-callback msg)
  (icymm-response msg "你骂人？！"))

;; qiushibaidu 的 html 太不标准了 :(
(define (icymm-joke-callback msg)
  (let ((seed (random 20))
        (found #f)
        (url ""))

    (condition-case 
    ;; 1. w3m online version
    ;; (with-input-from-pipe
    ;;  (format "w3m -dump http://www.qiushibaike.com")
    ;;  (lambda ()
    ;;    (let loop ((line (read-line))
    ;;               (count 0)
    ;;               (ret ""))
    ;;      (cond 
    ;;       ((string-search "^\\|" line)
    ;;        (if (= count seed)
    ;;            (if found
    ;;                ;; (string-append ret ", " url)
    ;;                (icymm-notice msg ret)
    ;;              (begin 
    ;;                (set! found #t)
    ;;                (loop (read-line) count "")))
    ;;          (loop (read-line) (+ count 1) "")))
    ;;       ((string-search "#([0-9]+) \\(!\\)" line)
    ;;        (set! url (format "http://www.qiushibaike.com/articles/~A.htm"
    ;;                          (last (string-search "#([0-9]+) \\(!\\)" line))))
    ;;        (loop (read-line) count ret))
    ;;       ((string-search "^[0-9]+ \\|" line)
    ;;        (loop (read-line) count ret))
    ;;       (else 
    ;;        (loop (read-line) count (string-append ret line)))))))

    ;; 2. w3m offline version
    (let ((s (with-input-from-pipe 
              (let* ((proxy-server (getenv "http_proxy"))
                     (proxy-port (getenv "http_port"))
                     (x (if (and proxy-server proxy-port)
                            (format "-x ~A:~A" proxy-server proxy-port)
                          "")))
                (format "curl ~A http://www.qiushibaike.com | w3m -dump -T text/html |  sed -e 's/支持.*\\|.*spacer.*\\|.*下一页.*\\|今日最糗.*\\|.*uc-logo.*//'"
                        x))
              read-string)))
      (icymm-notice
       msg 
       ;; (apply string-append 
       ;;        (string-split
       (string-trim-both
        (list-ref 
         (cdr 
          (reverse 
           (string-split-fields "\n.*\n\n\n\n\n" s #:infix)))
         seed))))
    ;; ))

    (err () 'ignored))))

;; TODO: 如何检测无限循环等问题？
(define (icymm-eval-callback msg)
  (let* ((body (irc:message-body msg))
         (positions (string-search-positions (regexp ",eval") body)))
    (with-input-from-string (substring body (cadr (car positions)))
      (lambda ()
        (let ((s (read)))
          (icymm-response
           msg
           (format "=> ~A"
                   ;; exn 是 exception 的标识。
                   (with-output-to-string
                     (lambda ()
                       (handle-exceptions exn "干啥呢，嘿嘿！" (eval s)))))))))))

(define (icymm-time-callback msg)
  (icymm-response msg (string-append "北京时间：" (seconds->string (current-seconds)))))

(define (icymm-uptime-callback msg)
  (icymm-response msg
                  (apply format "俺已经持续上线 ~A 天 ~A 小时 ~A 分又 ~A 秒啦！"
                         (let ((diff (inexact->exact (- (current-seconds) icymm-start-time))))
                           (list (quotient diff 86400)
                                 (quotient (remainder diff 86400) 3600)
                                 (quotient (remainder (remainder diff 86400) 3600) 60)
                                 (remainder (remainder (remainder diff 86400) 3600) 60))))))

(define (icymm-emms-callback msg)
  (icymm-response msg "Emacs 之超级音频、视频播放器！赶快来用吧！=> http://www.gnu.org/software/emms"))

(define (icymm-paste-callback msg)
  (icymm-response msg "贴贴贴！=> M-x xwl-paste-ubuntu-cn (http://paste.ubuntu.org.cn/46163), http://paste.ubuntu.org.cn (支持图片), wgetpaste"))

(define (icymm-url-callback msg)
  "Get title for url pasted in channel."
  (let* ((body (irc:message-body msg))
         (match (string-search "(https?://[^ ]+)" body)))
    (condition-case 
     (let* ((url (cadr match))
            (text (html->sxml (icymm-curl url)))
            (charset #f)
            (title
             (cadr 
              (string-search
               "([^\n ].*[^\n ])" 
               (car ((sxpath '(// title *text*)) text)))))
            (tiny-url 
             (if (> (string-length url) 50) ; magic..
                 (string-append 
                  (icymm-curl
                   (string-append 
                    "http://tinyurl.com/api-create.php?url=" url))
                  " ")
                  "")))

       (find (lambda (el) 
               (let ((m (string-search "charset *= *([^ ].+[^ ])" el)))
                 (if m (begin (set! charset (cadr m))
                              #t)
                   #f)))
             ((sxpath '(// meta @ content *text*))text))

       (when charset
         (set! title (icymm-iconv title charset 'utf-8)))

       (icymm-notice msg (string-append tiny-url title)))

     (err () 'ignored))))

(define (icymm-alias-callback msg)
  "Notify icymm aliases of people."
  (let* ((body (irc:message-body msg))
         (positions (string-search-positions (regexp ",alias (.+)") body)))
    (when positions
      (let ((aliases (string-split (apply substring body (cadr positions)))))
        (icymm-update-alias aliases)
        (icymm-response msg "aliases updated")))))

(define (icymm-update-alias aliases)
  (set! icymm-aliases
        (append (list aliases) icymm-aliases))
  (icymm-cache-save))

(define (icymm-weather-callback msg)
  (let* ((body (irc:message-body msg))
         (match (if (string-search ",w(eather)? *$" body)
                    (let ((sender (irc:message-sender msg)))
                      (list 
                       (or (icymm-guess-city
                            (icymm-get-ip-location (icymm-get-ip sender)))
                           "北京")))
                  (string-search ",w(eather)? +([^ ]+)" body))))
    (condition-case 
     (let* ((city (icymm-enca-as-gb18030 (last match)))
            (city-url (string-append
                       "http://search.weather.com.cn/static/url_gb.php?CityInfo=" 
                       city))
            (code (cadr (string-search "([0-9]{9})\\." (icymm-curl city-url)))))
       (icymm-notice msg (icymm-weather-generate-result  
                          (icymm-weather-prepare-data-from-sxml code)
                          ;; (icymm-weather-prepare-data-from-json city code)
                          (format "http://www.weather.com.cn/html/weather/~A.shtml" code)
                          )))
     (err () (begin (icymm-notice msg "City unknown or bad format")
                    'ignored)))))

(define (icymm-weather-generate-result data url)
  "(city weather1 temp1 weather2 temp2 weather3 temp3)."
  (apply format "~A: ~A ~A / 明天 ~A ~A / 后天 ~A ~A, ~A" (append data (list url))))

(define (icymm-weather-prepare-data-from-sxml code)
  (let* ((url (format "http://www.weather.com.cn/html/weather/~A.shtml" code))
         (sxml (html->sxml (icymm-curl url)))
         (city (icymm-weather-extract-city sxml))
         (temps (icymm-weather-extract-temperatures sxml))
         (phens (icymm-weather-extract-phenomenons sxml)))
    (let loop ((t temps)
               (p phens)
               (ret '()))
      (cond 
       ((null? t)
        (cons city (apply append (reverse ret))))
       ((= (length (car t)) 1)
        (loop (cdr t) (cdr p) (cons (list (car p) (string-append (caar t) "℃")) ret)))
       ((= (length (car t)) 2)
        (loop (cdr t) 
              (drop p 2)
              (cons (list 
                     (let ((w (take p 2)))
                       (if (string=? (car w) (cadr w))
                           (format "~A" (car w))
                         (apply format "~A~~~A" w)))
                     (apply format "~A℃~~~A℃" (car t)))
                    ret)))))))

(define (icymm-weather-extract-city sxml)
  (let ((city (find (lambda (el)
                      (let ((l (last el)))
                        (and (list? l) (eq? (car l) 'span))))
                    ((sxpath '(// div h1)) sxml))))
    (cadr city)))
            
(define (icymm-weather-extract-temperatures sxml)
  "Return a list of temperatures of following three days.
e.g.,  
  daytime: ((5 15) (20 7) (21 10))
  nighttime:  ((5) (20 7) (21 10))"
  (let* ((temps ((sxpath '(// strong *text*)) sxml))
         (first-temp-pos
          (let loop ((n 0)
                     (lst (cdr temps))
                     (el (car temps)))
            (if (string=? "℃" el)
                (- n 1)
              (loop (+ n 1) (cdr lst) (car lst))))))
    (map (lambda (el) (string-split el "℃"))
         (take (string-split 
                (apply string-append (drop temps first-temp-pos))
                "\r\n *") 3) )))

(define (icymm-weather-extract-phenomenons sxml)
  "Return of list of weather phenomenons for couples of days.

We have to look at `icymm-weather-extract-temperatures' to find the
corresponding phenomenon for each day."
  (let ((phenomenons ((sxpath '(// table tr td a *text*)) sxml)))
    (remove not
            (map (lambda (el)
                   (let ((m (string-search ".*target=\"_blank\">(.+)" el)))
                     (if m (cadr m) #f)))
                 phenomenons))))

(define (icymm-weather-prepare-data-from-json code)
  (let* ((json-url (format "http://m.weather.com.cn/data/~A.html" code))
         (json (with-input-from-string (icymm-curl json-url) json-read))
         (lst (vector->list (cdr (vector-ref json 0))))
         (matcher (lambda (match) (lambda (el) (string= (car el) match)))))
    (map (lambda (str) (cdr (find (matcher str) lst)))
         '("city" "weather1" "temp1" "weather2" "temp2" "weather3" "temp3"))))

(define (icymm-ip-callback msg)
  (let* ((body (irc:message-body msg))
         (match (string-search ",ip +([0-9.]{7,15})" body)))
    (condition-case
     (icymm-notice msg (icymm-get-ip-location (last match)))
     (err () (begin (icymm-notice msg "Bad format")
                    'ignored)))))

(define (icymm-seen-callback msg)
  (let* ((body (irc:message-body msg))
         (match (string-search 
                 (format ",seen (~A) ?" icymm-irc-nick-regexp) body)))
    (condition-case
     (let ((who (last match)))
       (if (icymm-user-online? who)
           (icymm-notice msg (format "Dude, ~A is currently online!" who))
         (icymm-notice msg (format "~A was last seen on: ~A."
                                   who (or (icymm-seen who) "No record")))))
     (err () (begin (icymm-notice msg "Bad format")
                    'ignored)))))

;; '((nick date)...)
(define icymm-seen-table '())

(define (icymm-seen who)
  (let ((r (assoc who icymm-seen-table)))
    (and r (format-date #f "~Y-~m-~d, ~H:~M:~S" (time->date (cadr r))))))

(define (icymm-quit-callback msg)
  (let* ((body (irc:message-body msg))
         (match (string-search
                 (format ":(~A)!.* (QUIT|PART)" icymm-irc-nick-regexp) body)))
    (condition-case
     (let* ((who (list-ref match 1))
            (record (assoc who icymm-seen-table)))
       (set! icymm-seen-table (cons (list who (current-time))
                                    (if record
                                        (remove record icymm-seen-table)
                                      icymm-seen-table)))
       (icymm-cache-save))
     (err () 'ignored))))

;; TODO, maybe provide ,unalias.

;;; Utilities

;; TODO, sum up enca, iconv

;; Convert STR to utf-8 encoded.
(define (icymm-enca-as-utf-8 str)
  (with-input-from-pipe 
   (string-append "echo " str " | enca -x utf-8")
   read-string))

(define (icymm-enca-as-gb18030 str)
  (with-input-from-pipe 
   (string-append "echo " str " | iconv -f utf-8 -t gb18030")
   read-string))

(define (icymm-iconv str from to)
  (with-input-from-pipe 
   (format "echo ~A | iconv -f ~A -t ~A | xargs echo -n " str from to)
   read-string))

(define (icymm-tell-timestamp)
  "11/08 11:53"
  (let ((lst (map (lambda (i)
                    (vector-ref (seconds->local-time (current-seconds)) i))
                  '(4 3 2 1))))
    (apply format #f "~2,'0D/~2,'0D ~2,'0D:~2,'0D" (cons (+ (car lst) 1) 
                                                         (cdr lst)))))
(define (icymm-curl url)
  ;; (with-input-from-request url #f read-string)
  (with-input-from-pipe
   (let ((proxy-server (getenv "http_proxy"))
         (proxy-port (getenv "http_port")))
     (if (and proxy-server proxy-port)
         (format "curl -x ~A:~A ~A" proxy-server proxy-port url)
       (format "curl ~A" url)))
   read-string))

(define (icymm-get-ip-location ip)
  (let ((loc (with-input-from-pipe 
              (format "ip_seek ~A ~A" icymm-ip-data ip)
              read-string)))
    (icymm-iconv (car (string-split loc "\n")) 'gbk 'utf-8)))

(define (icymm-guess-city str)
  ;; TODO: "省?" doesn't work, bug?
  (let ((match (or (string-search "省(.+)市" str)
                   (string-search "(.+)市" str))))
    (and match (last match))))

;;; Main

(define (icymm-load-rc)
  (let ((rc "~/.icymmrc"))
    (if (file-read-access? rc)
        (load rc)
      (with-output-to-file rc
        (lambda ()
          (display ";; -*- scheme -*-

(set! icymm-server \"irc.debian.org\")
(set! icymm-port 6667)
(set! icymm-fortune-file \"icymm.fortune\")
(set! icymm-logging-p #t)

(set! icymm-nick \"icymm\")
(set! icymm-channel \"#emacs-cn\")
(set! icymm-password \"\")
(set! icymm-real-name \"湘琴\")
"))))))

(define (icymm-parse-command-line)
  (let ((opts '()))
    (define (usage)
      (with-output-to-port (current-error-port)
        (lambda ()
          (print "Usage: " (car (argv)) " [OPTIONS...]")
          (newline)
          (print (args:usage opts))))
      (exit 1))

    (set! opts (list
                (args:make-option (h help) #:none "Show this help"
                                  (usage))
                (args:make-option (s server) #:required "Default is: irc.debian.org"
                                  (set! icymm-server arg))
                (args:make-option (p port) #:required "Default is: 6667"
                                  (set! icymm-port (string->number arg)))
                (args:make-option (n nick) #:required "Default is: icymm"
                                  (set! icymm-nick arg))
                (args:make-option (c channel) #:required "Default is: emacs-cn"
                                  (set! icymm-channel (string-append "#" arg)))
                (args:make-option (P password) #:required "Default is: #f"
                                  (set! icymm-password arg))))

    (args:parse (command-line-arguments) opts)))


(define (main)
  (icymm-load-rc)

  (icymm-parse-command-line)

  (icymm-tell-table-load)

  (parameterize ((tcp-read-timeout #f))
    (set! icymm-start-time (current-seconds))

    (set! icymm-connection (irc:connection server: icymm-server
                                           port: icymm-port
                                           nick: icymm-nick
                                           password: icymm-password
                                           real-name: icymm-real-name
                                           log-traffic: (when icymm-logging-p
                                                          (current-output-port))
                                           ))

    (irc:connect icymm-connection)

    (irc:join icymm-connection icymm-channel)

    (when icymm-password
      (irc:command icymm-connection (string-append "identify " icymm-password)))

    ;; privmsg
    (for-each (lambda (i) (apply icymm-add-privmsg-handler! i))
               `((",help"       ,icymm-help-callback    help)
                (",time"        ,icymm-time-callback    time)
                (",tell"        ,icymm-tell-callback    tell)
                (",uptime"      ,icymm-uptime-callback  uptime)
                (",你好"        ,icymm-你好-callback     nihao)
                (",joke"        ,icymm-joke-callback    joke)
                (",emms"        ,icymm-emms-callback    emms)
                (",paste"       ,icymm-paste-callback   paste)
                (".*https?://"  ,icymm-url-callback     url)
                (",alias"       ,icymm-alias-callback   alias)
                (",w(eather)?"  ,icymm-weather-callback weather)
                (",ip"          ,icymm-ip-callback      ip)
                (",seen"        ,icymm-seen-callback    seen)

                ("(靠|kao)[ \t!.。?]*$" ,icymm-dirty-callback dirty)

                ))

    (irc:add-message-handler! icymm-connection
                              icymm-join-callback
                              command: "JOIN"
                              tag: 'join)

    (irc:add-message-handler! icymm-connection
                              icymm-quit-callback
                              command: "QUIT"
                              tag: 'quit)

    (irc:add-message-handler! icymm-connection
                              icymm-quit-callback
                              command: "PART"
                              tag: 'part)

    ;; default 放在最后就可以了？
    (icymm-add-privmsg-handler! (format "~A: [^,]+|~A :[^,]+" icymm-nick icymm-nick)
                                icymm-default-callback
                                'default)

    ;; For debug+  Run this program in csi, then we can debug and modify it on the fly!!
    ;; (thread-start! (lambda () (irc:run-message-loop icymm-connection debug: #t)))

    ;; (irc:run-message-loop icymm-connection debug: #t)
    (irc:run-message-loop icymm-connection)
    ))

;; Let's go!
(main)


;;; icymm.scm ends here
