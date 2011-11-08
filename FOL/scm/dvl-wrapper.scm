(set! load/suppress-loading-message? #t)
(load "/home/manzyuk/Projects/BCL-AD/ls/dvl/load")
(pp (compile-to-raw-fol (read)))
(%exit 0)