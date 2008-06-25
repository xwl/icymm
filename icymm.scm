;;; icymm.scm --- an irc bot for #emacs-cn@irc.debian.org

;; Copyright (C) 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

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

(use posix irc format-modular regex srfi-1)

;; config
(define icymm-server "irc.debian.org")
(define icymm-nick "icymm")
(define icymm-channel "#emacs-cn")

(define icymm-connection (irc:connection server: icymm-server nick: icymm-nick))

;;; Data Table

(define (icymm-receiver-is-me? receiver)
  (equal? receiver (irc:connection-nick icymm-connection)))

;; '(receiver sender msg)
(define icymm-tell-table
  '())

(define (icymm-tell-table-add! receiver sender msg)
  (set! icymm-tell-table
        (cons (list receiver sender msg) icymm-tell-table)))

(define (icymm-tell-table-remove! receiver)
  (set! icymm-tell-table
        (alist-delete receiver icymm-tell-table)))

(define (icymm-response msg response)
;;   (display (format "(sender, receiver): (~A, ~A)\n"
;;                    (irc:message-sender msg)
;;                    (irc:message-receiver msg)))
;;   (display (format "body: ~A\n" (irc:message-body msg)))
  (if (icymm-receiver-is-me? (irc:message-receiver msg))
      (irc:say icymm-connection response (irc:message-sender msg))
      (irc:say icymm-connection response)))


;;; Callbacks

(define (icymm-help-callback msg)
  (icymm-response msg "支持的命令：,help ,time ,emacs-cn ,tell ,joke"))

(define (icymm-time-callback msg)
  (icymm-response msg (string-append "东京时间：" (seconds->string (current-seconds)))))

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
  (icymm-response msg "没记错的话，它是：http://www.emacs.cn"))

(define (icymm-tell-callback msg)
  "发离线消息。"
  (let* ((sender (irc:message-sender msg))
         (body (irc:message-body msg))
         (positions (string-match-positions (regexp ",tell ([a-zA-Z_]+)") body)))
    (when positions
          (let ((future-receiver (apply substring body (cadr positions)))
                (content (substring body (cadr (car positions)))))
            (icymm-tell-table-add! future-receiver sender content)
            (icymm-response msg
                              (format "知道了，帮你记下了。下次 ~A 上线的时候，代为转告。"
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
                    (icymm-tell-table-remove! who)
                    (loop)))))))

(define (icymm-你好-callback msg)
  (icymm-response msg "好，好，大家好！"))

(define (icymm-dirty-callback msg)
  (icymm-response msg "你骂人？！"))

(define (icymm-joke-callback msg)
  (icymm-response msg "帮你找笑话-ing，耐心等会哦…(TODO)")
  ;; TODO, get html
  )


;;; Main

(define (main)
  (parameterize
   ((tcp-read-timeout #f))
   (irc:connect icymm-connection)

   (irc:join icymm-connection icymm-channel)

   (irc:add-message-handler! icymm-connection
                             icymm-help-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm.*,help")
                                      (irc:message-body msg))))

   (irc:add-message-handler! icymm-connection
                             icymm-time-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm.*,time")
                                      (irc:message-body msg))))

   (irc:add-message-handler! icymm-connection
                             icymm-emacs-cn-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm.*,emacs-cn")
                                      (irc:message-body msg))))

   (irc:add-message-handler! icymm-connection
                             icymm-tell-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm.*,tell")
                                      (irc:message-body msg))))

   (irc:add-message-handler! icymm-connection
                             icymm-join-callback
                             command: "JOIN")

   (irc:add-message-handler! icymm-connection
                             icymm-你好-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm.*你好")
                                      (irc:message-body msg))))

   (irc:add-message-handler! icymm-connection
                             icymm-dirty-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm.*靠")
                                      (irc:message-body msg))))

   (irc:add-message-handler! icymm-connection
                             icymm-joke-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm.*,joke")
                                      (irc:message-body msg))))

   ;; default 放在最后就可以了？
   (irc:add-message-handler! icymm-connection
                             icymm-default-callback
                             command: "PRIVMSG"
                             body: (lambda (msg)
                                     (string-match
                                      (regexp "PRIVMSG.*icymm[^,]+")
                                      (irc:message-body msg))))

   (irc:run-message-loop icymm-connection debug: #t)))

;; Let's go!
(main)


;;; icymm.scm ends here
