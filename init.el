;; This a sample emacs init file ~/.emacs or ~/.emacs.d/init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ers-env-variables
   (quote
    (("JAVA_HOME" . "/Library/Java/JavaVirtualMachines/jdk1.7.0_45.jdk/Contents/Home")
     ("EDITOR" . "emacsclient")
     ("LC_ALL" . "C")
     ("LANG" . "en"))))
 '(ers-erc-channel-list
   (quote
    (("freenode.net" "##java" "#freebsd" "#grails" "#emacs"))))
 '(ers-powerline-enabled t)
 '(ers-tab-size 4)
 '(ers-ui-font "Monospace-12")
 '(ers-ui-theme (quote base16-solarized))
 '(ers-ui-theme-console (quote base16-default))
 '(inhibit-startup-screen t)
 '(personal-blog-name "wordpress-me_in_dot_authinfo")
 '(corporate-blog-name "wordpress-myorganization_in_dot_authinfo")
 '(ers-pim-mail-address "me@gmail.com")
 '(ers-pim-public-mail-address "me.spam@gmail.com")
 '(ers-pim-organization "Me")
 '(ers-pim-full-name "Me me")
 '(personal-blog-url "http://me.wordpress.com/xmlrpc.php")
 '(corporate-blog-url "http://me.com/xmlrpc.php")
 '(send-mail-function (quote smtpmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'cl-lib)
(require 'org)
(require 'ob-tangle)

;; Just for testing, for the typical setup see the README file, use the full path to startup.org..
(org-babel-load-file (expand-file-name
                      (concat
                       (file-name-as-directory
                        (concat (file-name-as-directory (file-name-directory load-file-name)) "bootstrap"))
                       "startup.org")))
