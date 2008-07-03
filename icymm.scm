;;; icymm.scm --- an irc bot for #emacs-cn@irc.debian.org

;; Copyright (C) 2008 William Xu

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

(use posix tcp irc format-modular regex srfi-1)

;; config
(define icymm-server "irc.debian.org")
(define icymm-nick "icymm")
(define icymm-channel "#emacs-cn")

(define icymm-connection (irc:connection server: icymm-server nick: icymm-nick))
(define icymm-start-time #f)

;;; Data Table

(define (icymm-receiver-is-me? receiver)
  (equal? receiver (irc:connection-nick icymm-connection)))

;; '(receiver sender msg)
(define icymm-tell-table
  '())

(define (icymm-tell-table-add! receiver sender msg)
  (set! icymm-tell-table
        (append icymm-tell-table (list (list receiver sender msg)))))

(define (icymm-tell-table-remove! record)
  ;; Not alist-delete here, since a user may recieve multiple messages
  ;; from different people.
  (set! icymm-tell-table (delete record icymm-tell-table)))

(define (icymm-response msg response)
;;   (display (format "(sender, receiver): (~A, ~A)\n"
;;                    (irc:message-sender msg)
;;                    (irc:message-receiver msg)))
;;   (display (format "body: ~A\n" (irc:message-body msg)))
  (if (icymm-receiver-is-me? (irc:message-receiver msg))
      (irc:say icymm-connection response (irc:message-sender msg))
      (irc:say icymm-connection response)))

(define (icymm-add-privmsg-handler! command callback)
 (irc:add-message-handler! icymm-connection
                           callback
                           command: "PRIVMSG"
                           body: (lambda (msg)
                                   (string-match
                                    (regexp (format "PRIVMSG.*(:| )~A:.*~A" icymm-nick command))
                                    (irc:message-body msg)))))

(define (icymm-format-url url)
  "在 URL 前加一些前缀。"
 (string-append "没记错的话，它是：" url))


;;; Callbacks

(define (icymm-help-callback msg)
  (icymm-response msg "支持的命令：,help ,time ,emacs-cn ,tell ,joke ,uptime ,emms ,paste ..."))

(define (icymm-default-callback msg)
  (let* ((db '("不懂你在说什么呃… :P"
               "确认你说的是人话？！"
               "嗯？"
               "哦。"
               "嗯"
               ":-)"
               ":P"
               "今天天气挺不错。"
               "一天到晚游泳的鱼。"
               "饭没，兄弟？"
               ))
         (i (random (length db))))
    (icymm-response msg (list-ref db i))))

(define (icymm-emacs-cn-callback msg)
  (icymm-response msg  (icymm-format-url "http://www.emacs.cn")))

(define (icymm-tell-callback msg)
  "发离线消息。"
  (let* ((sender (irc:message-sender msg))
         (body (irc:message-body msg))
         (positions (string-match-positions (regexp ",tell ([a-zA-Z_]+) ?") body)))
    (when positions
          (let ((future-receiver (apply substring body (cadr positions)))
                (content (substring body (cadr (car positions)))))
            (icymm-tell-table-add! future-receiver sender content)
            (icymm-response msg
                              (format "知道了，帮你记下了。下次 ~A 上线的时候，代为转告咯！"
                                      future-receiver))))))

(define (icymm-join-callback msg)
  (let* ((body (irc:message-body msg))
         (positions (string-match-positions (regexp ":([a-zA-Z_]+)!") body)))
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
  (icymm-response msg "帮你找笑话-ing，耐心等会哦…(TODO)")
  ;; TODO, get html
  )

;; TODO: 如何检测无限循环等问题？
(define (icymm-eval-callback msg)
  (let* ((body (irc:message-body msg))
         (positions (string-match-positions (regexp ",eval") body)))
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
  (icymm-response msg (string-append "东京时间：" (seconds->string (current-seconds)))))

(define (icymm-uptime-callback msg)
  (icymm-response msg
                  (apply format "俺已经持续上线 ~A 天 ~A 小时 ~A 分又 ~A 秒啦！"
                         (let ((diff (inexact->exact (- (current-seconds) icymm-start-time))))
                           (list (quotient diff 86400)
                                 (quotient (remainder diff 86400) 3600)
                                 (quotient (remainder (remainder diff 86400) 3600) 60)
                                 (remainder (remainder (remainder diff 86400) 3600) 60))))))

(define (icymm-emms-callback msg)
  (icymm-response msg "Emacs 中的超级音频、视频播放器！赶快来用吧！=> http://www.gnu.org/software/emms"))

(define (icymm-paste-callback msg)
  (icymm-response msg "想贴好多好多哦？来这里，让你一次贴个够！=> wgetpaste, http://paste.lisp.org/, http://paste.ubuntu.org.cn (支持图片)"))


;;; Main

(define (main)
  (parameterize
   ((tcp-read-timeout #f))

   (set! icymm-start-time (current-seconds))

   (irc:connect icymm-connection)

   (irc:join icymm-connection icymm-channel)

   (icymm-add-privmsg-handler! ",help"     icymm-help-callback)
   (icymm-add-privmsg-handler! ",time"     icymm-time-callback)
   (icymm-add-privmsg-handler! ",emacs-cn" icymm-emacs-cn-callback)
   (icymm-add-privmsg-handler! ",tell"     icymm-tell-callback)
   (icymm-add-privmsg-handler! ",uptime"   icymm-uptime-callback)
   (icymm-add-privmsg-handler! ",你好"     icymm-你好-callback)
   (icymm-add-privmsg-handler! "靠"        icymm-dirty-callback)
   (icymm-add-privmsg-handler! ",joke"     icymm-joke-callback)
   ;; (icymm-add-privmsg-handler  ",eval"     icymm-eval-callback)
   (icymm-add-privmsg-handler! ",emms"     icymm-emms-callback)
   (icymm-add-privmsg-handler! ",paste"    icymm-paste-callback)

   (irc:add-message-handler! icymm-connection
                             icymm-join-callback
                             command: "JOIN")

   ;; default 放在最后就可以了？
   (icymm-add-privmsg-handler! "[^,]+" icymm-default-callback)

   (irc:run-message-loop icymm-connection debug: #t)))

;; Let's go!
(main)


;;; icymm.scm ends here
