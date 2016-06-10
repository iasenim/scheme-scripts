(library (dir-utils)
  (export leaf-directories)
  (import (chezscheme))


  (define expand-directory-list
    (lambda (root)
      (map (lambda (x) (string-append root "/" x)) (directory-list root))))
  
  ;; If we look at a directory tree, and take the deepest directories as the leaves,
  ;; (leaf-directories "some_directory") will return a list of paths of these leaves
  ;; Example:
  ;;          dir1
  ;;         /    \
  ;;        dir2  dir3
  ;;       /  \     \
  ;;     dir4  dir5  dir6
  ;;     /      \       \
  ;;    file1    file2   file3
  ;; (leaf-directories "dir1") will return ("dir1/dir2/dir4" "dir1/dir2/dir5" "dir1/dir3/dir6")
  
  (define leaf-directories
    (lambda (root)
      (if (not (file-directory? root))
          '()
          (let ([children (expand-directory-list root)])
            (if (for-all file-regular? children)
                (list root)
                (fold-left append '() (map leaf-directories children)))))))
  )

