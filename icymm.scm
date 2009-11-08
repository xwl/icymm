;;; icymm.scm --- an irc bot for #emacs-cn@irc.debian.org

;; Copyright (C) 2008 2009 William Xu

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
     http-client html-parser sxpath format)

;;; Global Variables
(define icymm-server "irc.debian.org")
(define icymm-nick "icymm")
(define icymm-channel "#emacs-cn")
(define icymm-password #f)
(define icymm-real-name "湘琴")

(define icymm-connection #f)
(define icymm-start-time #f)


;;; Data Table

(define (icymm-receiver-is-me? receiver)
  (equal? receiver (irc:connection-nick icymm-connection)))

;; '(receiver sender msg)
(define icymm-tell-table '())

(define icymm-tell-table-cache-file "~/.icymm-cache")

(define (icymm-tell-table-save)
  "Save `icymm-tell-table' to `icymm-tell-table-cache-file'."
  (with-output-to-file icymm-tell-table-cache-file
    (lambda ()
      (print
       (format
        ";; -*- scheme -*-

 (set! icymm-tell-table '~S)"
        icymm-tell-table)))))

(define (icymm-tell-table-load)
  "Load `icymm-tell-table' from `icymm-tell-table-cache-file'."
  (when (file-read-access? icymm-tell-table-cache-file)
    (load icymm-tell-table-cache-file)))

(define (icymm-tell-table-add! receiver sender msg)
  (set! icymm-tell-table
        (append icymm-tell-table (list (list receiver sender msg))))
  (icymm-tell-table-save))

(define (icymm-tell-table-remove! record)
  ;; Not alist-delete here, since a user may recieve multiple messages
  ;; from different people.
  (set! icymm-tell-table (delete record icymm-tell-table))
  (icymm-tell-table-save))

(define (icymm-response msg response)
;;   (display (format "(sender, receiver): (~A, ~A)\n"
;;                    (irc:message-sender msg)
;;                    (irc:message-receiver msg)))
;;   (display (format "body: ~A\n" (irc:message-body msg)))
  (if (icymm-receiver-is-me? (irc:message-receiver msg))
      (irc:say icymm-connection response (irc:message-sender msg))
      (irc:say icymm-connection response)))

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


;;; Callbacks

(define (icymm-format-url url)
  "在 URL 前加一些前缀。"
 (string-append "没记错的话，它是：" url))

(define (icymm-help-callback msg)
  (icymm-response msg "支持的命令：,help ,time ,emacs-cn ,tell ,uptime ,emms ,paste ..."))
;; TODO fix ,joke 

(define (icymm-default-callback msg)
  ;; (let* ((db '("何當共剪西窗燭 卻話巴山夜時雨"
  ;;              "若見諸相非相 則見如來"
  ;;              "本來無一物 何處惹塵埃"
  ;;              "一日不見 如隔三秋"
  ;;              "がんばりましょう！"
  ;;              "朝聞道 夕死可矣"
  ;;              "亡之，命矣夫！斯人也而有斯疾也！斯人也而有斯疾也!"
  ;;              ))
  ;;        (i (random (length db))))
  ;;   (icymm-response msg (list-ref db i))))
  (letrec ((fortune-reader
            (lambda (s)
              (let ((tmp (read-line)))
                (if (eof-object? tmp)
                    s
                  (fortune-reader (string-append s tmp " ")))))))
    (icymm-response 
     msg 
     (with-input-from-pipe "fortune" (lambda () (fortune-reader ""))))))

(define (icymm-emacs-cn-callback msg)
  (icymm-response msg  (icymm-format-url "http://www.emacs.cn")))

(define (icymm-tell-callback msg)
  "发离线消息。"
  (let* ((sender (irc:message-sender msg))
         (body (irc:message-body msg))
         (positions (string-search-positions (regexp ",tell ([a-zA-Z_]+) ?") body)))
    (when positions
      (let ((future-receiver (apply substring body (cadr positions)))
            (content (substring body (cadr (car positions)))))
        (icymm-tell-table-add! 
         future-receiver 
         sender 
         (string-append "[" (icymm-tell-timestamp) "] " content))
        (icymm-response msg
                        (format "收到，下次 ~A 上线的时候，代为转告！"
                                future-receiver))))))

(define (icymm-join-callback msg)
  (let* ((body (irc:message-body msg))
         (positions (string-search-positions (regexp ":([a-zA-Z_]+)!") body)))
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

(define (icymm-joke-callback msg)
  ;; (icymm-response msg "帮你找笑话-ing，耐心等会哦…")
  (let ((max-tries 3))
    (define (iter try)
      (if (> try max-tries)
          "你 rp 不行啊，这回竟然没找到笑话…"
          (with-input-from-pipe
           (format "w3m -dump http://www.qiushibaike.com/qiushi/number/~A.html" (random 30000))
           (lambda ()
             (let loop ((beg #f)
                        (end #f)
                        (line "")
                        (ret ""))
               (set! line (read-line))
               (cond
                ((or end (string-search (regexp "很抱歉，糗事#[0-9]+不存在") line))
                 (if (string-null? ret)
                     (iter (+ 1 try))
                     ret))
                ((string-search (regexp "< 上一糗事") line)
                 (loop beg #t line ret))
                (beg
                 (loop beg end line (string-append ret line)))
                ((string-search (regexp "糗事#[0-9]+") line)
                 (read-line)            ; skip date
                 (loop #t end line (string-append ret "(" line ") ")))
                (else
                 (loop beg end line ret))))))))
    (icymm-response msg (iter 0))))

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
         (positions (string-search-positions (regexp "(https?://[^ ]+) ?") body)))
    (when positions
      (let ((url (apply substring body (cadr positions))))
        (condition-case 
         (icymm-response 
          msg
          (icymm-enca-as-utf-8
           (car ((sxpath `(head title *text*))
                 (html->sxml (with-input-from-request url #f read-string))))))
         (err () 'ignored))))))


;;; Utilities

;; Convert STR to utf-8 encoded.
(define (icymm-enca-as-utf-8 str)
  (with-input-from-pipe 
   (string-append "echo " str " | enca -x utf-8")
   read-string))

(define (icymm-tell-timestamp)
  "11/08 11:53"
  (let ((lst (map (lambda (i)
                    (vector-ref (seconds->local-time (current-seconds)) i))
                  '(4 3 2 1))))
    (apply format #f "~2,'0D/~2,'0D ~2,'0D:~2,'0D" (cons (+ (car lst) 1) 
                                                         (cdr lst)))))


;;; Main

(define (icymm-load-rc)
  (let ((rc "~/.icymmrc"))
    (if (file-read-access? rc)
        (load rc)
      (with-output-to-file rc
        (lambda ()
          (display ";; -*- scheme -*-

 (set! icymm-server \"irc.debian.org\")
 (set! icymm-nick \"icymm\")
 (set! icymm-channel \"#emacs-cn\")
 (set! icymm-password \"bot password\")
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
                (args:make-option (n nick) #:required "Default is: icymm"
                                  (set! icymm-nick arg))
                (args:make-option (c channel) #:required "Default is: #emacs-cn"
                                  (set! icymm-channel arg))
                (args:make-option (p password) #:required "Default is: #f"
                                  (set! icymm-password arg))))

    (args:parse (command-line-arguments) opts)))

(define (main)
  (icymm-load-rc)

  (icymm-parse-command-line)

  (icymm-tell-table-load)

  (parameterize ((tcp-read-timeout #f))
    (set! icymm-start-time (current-seconds))

    (set! icymm-connection (irc:connection server: icymm-server
                                           nick: icymm-nick
                                           password: icymm-password
                                           real-name: icymm-real-name))
    (irc:connect icymm-connection)

    (irc:join icymm-connection icymm-channel)

    (when icymm-password
      (irc:command icymm-connection (string-append "identify " icymm-password)))

    (for-each (lambda (i) (apply icymm-add-privmsg-handler! i))
              `((",help"     ,icymm-help-callback     help)
                (",time"     ,icymm-time-callback     time)
                (",emacs-cn" ,icymm-emacs-cn-callback emacs-cn)
                (",tell"     ,icymm-tell-callback     tell)
                (",uptime"   ,icymm-uptime-callback   uptime)
                (",你好"     ,icymm-你好-callback     nihao)
                ("靠"        ,icymm-dirty-callback    dirty)
                (",joke"     ,icymm-joke-callback     joke)
                (",emms"     ,icymm-emms-callback     emms)
                (",paste"    ,icymm-paste-callback    paste)
                ("https?://" ,icymm-url-callback      url)
                ))

    (irc:add-message-handler! icymm-connection
                              icymm-join-callback
                              command: "JOIN"
                              tag: 'join)

    ;; default 放在最后就可以了？
    (icymm-add-privmsg-handler! (format "~A: [^,]+|~A :[^,]+" icymm-nick icymm-nick)
                                icymm-default-callback
                                'default)

    ;; For debug+  Run this program in csi, then we can debug and modify it on the fly!!
    ;; (thread-start! (lambda () (irc:run-message-loop icymm-connection debug: #t)))
    (irc:run-message-loop icymm-connection debug: #t)

    ))

;; Let's go!
(main)


;;; icymm.scm ends here
