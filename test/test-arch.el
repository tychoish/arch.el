;;; test-arch.el --- ERT test suite for arch.el -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; URL: https://github.com/tychoish/arch.el

;;; Commentary:
;; Headless ERT unit tests for arch, arch-sets, and arch-elpa.
;; CLI calls are mocked to allow test execution on non-Arch systems.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'arch)
(require 'arch-sets)
(require 'arch-elpa)

;;; 1. Backend Protocol Tests

(ert-deftest arch-test-backend-registration-and-lookup ()
  "Test backend creation, registration, and registry lookup."
  (let ((mock-backend (arch-backend--make
                       :name "mock-mgr"
                       :label "Mock Manager"
                       :search-fn (lambda (_q) nil)
                       :info-fn (lambda (_p) nil)
                       :list-fn (lambda () nil))))
    (arch-register-backend mock-backend)
    (let ((retrieved (map-elt arch--backends "mock-mgr")))
      (should retrieved)
      (should (equal (arch-backend-name retrieved) "mock-mgr"))
      (should (equal (arch-backend-label retrieved) "Mock Manager")))))

(ert-deftest arch-test-default-and-aur-backend-resolution ()
  "Test resolving default and AUR backends and error signaling."
  (let ((arch-default-backend "pacman")
        (arch-aur-backend "yay"))
    (should (arch--default-backend))
    (should (equal (arch-backend-name (arch--default-backend)) "pacman"))
    (should (arch--aur-backend))
    (should (equal (arch-backend-name (arch--aur-backend)) "yay"))
    (let ((arch-default-backend "non-existent-mgr"))
      (should-error (arch--default-backend) :type 'user-error))
    (let ((arch-aur-backend "non-existent-aur"))
      (should-error (arch--aur-backend) :type 'user-error))))

;;; 2. Package Parsing Tests

(ert-deftest arch-test-parse-search-output ()
  "Test parsing pacman -Ss stdout into arch-pkg structures."
  (let* ((sample-output
          (concat "core/bash 5.2.026-1 [installed]\n"
                  "    The GNU Bourne Again shell\n"
                  "extra/ripgrep 14.1.0-1\n"
                  "    A search tool that combines the usability of ag with the raw speed of grep\n"))
         (pkgs (arch--parse-search-output sample-output)))
    (should (= (length pkgs) 2))
    ;; First package
    (let ((pkg1 (nth 0 pkgs)))
      (should (equal (arch-pkg-name pkg1) "bash"))
      (should (equal (arch-pkg-repo pkg1) "core"))
      (should (equal (arch-pkg-version pkg1) "5.2.026-1"))
      (should (arch-pkg-installed-p pkg1))
      (should (equal (arch-pkg-description pkg1) "The GNU Bourne Again shell")))
    ;; Second package
    (let ((pkg2 (nth 1 pkgs)))
      (should (equal (arch-pkg-name pkg2) "ripgrep"))
      (should (equal (arch-pkg-repo pkg2) "extra"))
      (should (equal (arch-pkg-version pkg2) "14.1.0-1"))
      (should-not (arch-pkg-installed-p pkg2))
      (should (string-prefix-p "A search tool" (arch-pkg-description pkg2))))))

(ert-deftest arch-test-parse-info-output ()
  "Test parsing pacman -Qi/-Si key-value output with continuation lines."
  (let* ((sample-info
          (concat "Name            : ripgrep\n"
                  "Version         : 14.1.0-1\n"
                  "Description     : A search tool that combines ag and grep\n"
                  "Architecture    : x86_64\n"
                  "URL             : https://github.com/BurntSushi/ripgrep\n"
                  "Licenses        : MIT  UNLICENSE\n"
                  "Depends On      : glibc  gcc-libs  pcre2\n"
                  "                  libunwind\n"
                  "Installed Size  : 5.24 MiB\n"))
         (plist (arch--parse-info-output sample-info)))
    (should (equal (plist-get plist 'name) "ripgrep"))
    (should (equal (plist-get plist 'version) "14.1.0-1"))
    (should (equal (plist-get plist 'url) "https://github.com/BurntSushi/ripgrep"))
    (should (equal (plist-get plist 'architecture) "x86_64"))
    ;; Multi-line continuation test
    (should (equal (plist-get plist 'depends-on) "glibc  gcc-libs  pcre2  libunwind"))))

(ert-deftest arch-test-parse-multi-info ()
  "Test parsing multiple package info records separated by double newlines."
  (let* ((sample-multi
          (concat "Name            : pkg-a\n"
                  "Version         : 1.0\n\n"
                  "Name            : pkg-b\n"
                  "Version         : 2.0\n"))
         (records (arch--parse-multi-info sample-multi)))
    (should (= (length records) 2))
    (should (equal (plist-get (nth 0 records) 'name) "pkg-a"))
    (should (equal (plist-get (nth 1 records) 'name) "pkg-b"))))

(ert-deftest arch-test-parse-installed-output ()
  "Test parsing pacman -Q output."
  (let* ((sample-installed "curl 8.5.0-1\ngit 2.43.0-1\n")
         (pkgs (arch--parse-installed-output sample-installed)))
    (should (= (length pkgs) 2))
    (should (equal (arch-pkg-name (nth 0 pkgs)) "curl"))
    (should (equal (arch-pkg-version (nth 0 pkgs)) "8.5.0-1"))
    (should (arch-pkg-installed-p (nth 0 pkgs)))
    (should (equal (arch-pkg-name (nth 1 pkgs)) "git"))
    (should (equal (arch-pkg-version (nth 1 pkgs)) "2.43.0-1"))
    (should (arch-pkg-installed-p (nth 1 pkgs)))))

(ert-deftest arch-test-parse-sync-list ()
  "Test parsing pacman -Sl output."
  (let* ((sample-sync "extra curl 8.5.0-1 [installed]\ncore bash 5.2.026-1\n")
         (pkgs (arch--parse-sync-list sample-sync)))
    (should (= (length pkgs) 2))
    (should (equal (arch-pkg-name (nth 0 pkgs)) "curl"))
    (should (equal (arch-pkg-repo (nth 0 pkgs)) "extra"))
    (should (arch-pkg-installed-p (nth 0 pkgs)))
    (should (equal (arch-pkg-name (nth 1 pkgs)) "bash"))
    (should (equal (arch-pkg-repo (nth 1 pkgs)) "core"))
    (should-not (arch-pkg-installed-p (nth 1 pkgs)))))

;;; 3. Package Sets Data Structures and Import/Export Tests

(ert-deftest arch-test-sets-format-version ()
  "Verify package-set format version constant."
  (should (= arch-sets-format-version 1)))

(ert-deftest arch-test-sets-entry-to-alist ()
  "Test conversion of (name . source) entry to alist representation."
  (let ((entry (cons "ripgrep" "pacman")))
    (should (equal (arch-sets--entry-to-alist entry)
                   '((name . "ripgrep") (source . "pacman"))))))

(ert-deftest arch-test-sets-resolve-backend-name ()
  "Test resolving backend names for standard sources pacman, aur, and custom db."
  (let ((arch-aur-backend "yay"))
    (should (equal (arch-sets--resolve-backend-name '((name . "foo") (source . "pacman"))) "pacman"))
    (should (equal (arch-sets--resolve-backend-name '((name . "bar") (source . "aur"))) "yay"))
    (should (equal (arch-sets--resolve-backend-name '((name . "baz") (source . "db") (backend . "custom"))) "custom"))
    (should-not (arch-sets--resolve-backend-name '((name . "quux") (source . "unsupported"))))))

(ert-deftest arch-test-sets-group-by-backend ()
  "Test grouping package entries by resolved backend."
  (let* ((arch-aur-backend "yay")
         (entries '(((name . "curl") (source . "pacman"))
                    ((name . "ripgrep") (source . "pacman"))
                    ((name . "yay-git") (source . "aur"))))
         (groups (arch-sets--group-by-backend entries)))
    (should (hash-table-p groups))
    (let ((pacman-pkgs (map-elt groups "pacman"))
          (yay-pkgs (map-elt groups "yay")))
      (should (= (length pacman-pkgs) 2))
      (should (member "curl" pacman-pkgs))
      (should (member "ripgrep" pacman-pkgs))
      (should (equal yay-pkgs '("yay-git"))))))

(ert-deftest arch-test-sets-status-and-row ()
  "Test package set status calculation and row formatting."
  (let ((installed (make-hash-table :test #'equal))
        (entry-inst '((name . "curl") (source . "pacman") (backend . "pacman")))
        (entry-miss '((name . "missing-pkg") (source . "pacman") (backend . "pacman"))))
    (puthash "curl" t installed)
    (should (arch-sets--entry-installed-p entry-inst installed))
    (should-not (arch-sets--entry-installed-p entry-miss installed))
    (let ((row-inst (arch-sets--build-entry-row entry-inst installed))
          (row-miss (arch-sets--build-entry-row entry-miss installed)))
      (should (equal (car row-inst) entry-inst))
      (should (string-match-p "installed" (aref (cadr row-inst) 2)))
      (should (string-match-p "missing" (aref (cadr row-miss) 2))))))

(ert-deftest arch-test-sets-yaml-round-trip ()
  "Test serializing and deserializing package set data."
  (let ((temp-file (make-temp-file "arch-sets-test-" nil ".yaml"))
        (test-data (list (cons 'version 1)
                         (cons 'host "test-host")
                         (cons 'packages
                               (list '((name . "emacs") (source . "pacman"))
                                     '((name . "paru-bin") (source . "aur")))))))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert (yaml-encode test-data) "\n"))
          (let ((parsed-pkgs (arch-sets--parse-file-packages temp-file)))
            (should (= (length parsed-pkgs) 2))
            (should (equal (alist-get 'name (nth 0 parsed-pkgs)) "emacs"))
            (should (equal (alist-get 'source (nth 0 parsed-pkgs)) "pacman"))
            (should (equal (alist-get 'name (nth 1 parsed-pkgs)) "paru-bin"))
            (should (equal (alist-get 'source (nth 1 parsed-pkgs)) "aur"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; 4. Arch-ELPA List and Table Logic Tests

(ert-deftest arch-test-elpa-pkg-make ()
  "Test arch-elpa-pkg structure construction and accessors."
  (let ((pkg (arch-elpa-pkg--make
              :name 'magit
              :version "3.3.0"
              :archive "melpa"
              :summary "A Git porcelain inside Emacs"
              :installed-p t
              :upgradeable-p nil
              :built-in-p nil)))
    (should (eq (arch-elpa-pkg-name pkg) 'magit))
    (should (equal (arch-elpa-pkg-version pkg) "3.3.0"))
    (should (equal (arch-elpa-pkg-archive pkg) "melpa"))
    (should (arch-elpa-pkg-installed-p pkg))
    (should-not (arch-elpa-pkg-upgradeable-p pkg))
    (should-not (arch-elpa-pkg-built-in-p pkg))))

(ert-deftest arch-test-elpa-pkg-status ()
  "Test status string formatting for elpa packages."
  (let ((pkg-builtin (arch-elpa-pkg--make :name 'project :built-in-p t :installed-p nil))
        (pkg-inst (arch-elpa-pkg--make :name 'magit :built-in-p nil :installed-p t))
        (pkg-avail (arch-elpa-pkg--make :name 'gptel :built-in-p nil :installed-p nil)))
    (should (string-match-p "built-in" (arch-elpa--pkg-status pkg-builtin)))
    (should (string-match-p "installed" (arch-elpa--pkg-status pkg-inst)))
    (should (string-match-p "avail" (arch-elpa--pkg-status pkg-avail)))))

(ert-deftest arch-test-elpa-build-entry ()
  "Test tabulated-list row construction for arch-elpa."
  (let* ((pkg (arch-elpa-pkg--make
               :name 'corfu
               :version "1.0"
               :archive "melpa"
               :summary "Completion Overlay Region Function"
               :installed-p t
               :upgradeable-p t
               :built-in-p nil))
         (arch-elpa--marked (make-hash-table :test #'equal)))
    ;; Unmarked
    (let ((entry (arch-elpa--build-entry pkg)))
      (should (equal (car entry) pkg))
      (let ((vec (cadr entry)))
        (should (equal (aref vec 1) "melpa"))
        (should (string-match-p "installed" (aref vec 2)))
        (should (equal (aref vec 4) "Completion Overlay Region Function"))))
    ;; Marked
    (puthash 'corfu t arch-elpa--marked)
    (let ((entry-marked (arch-elpa--build-entry pkg)))
      (should (eq (get-text-property 0 'face (aref (cadr entry-marked) 0))
                  'arch-face-pkg-link-marked)))))

(ert-deftest arch-test-elpa-filter-predicates ()
  "Test arch-elpa filter predicates."
  (let ((pkg-up (arch-elpa-pkg--make :upgradeable-p t :installed-p t :built-in-p nil))
        (pkg-in (arch-elpa-pkg--make :upgradeable-p nil :installed-p t :built-in-p nil))
        (pkg-av (arch-elpa-pkg--make :upgradeable-p nil :installed-p nil :built-in-p nil))
        (pkg-bi (arch-elpa-pkg--make :upgradeable-p nil :installed-p nil :built-in-p t)))
    (should (arch-elpa--filter-upgradeable-p pkg-up))
    (should-not (arch-elpa--filter-upgradeable-p pkg-in))
    (should (arch-elpa--filter-installed-p pkg-in))
    (should-not (arch-elpa--filter-installed-p pkg-av))
    (should (arch-elpa--filter-available-p pkg-av))
    (should-not (arch-elpa--filter-available-p pkg-in))
    (should (arch-elpa--filter-built-in-p pkg-bi))
    (should-not (arch-elpa--filter-built-in-p pkg-av))))

;;; 5. Mock CLI Pacman/Yay Calls (Headless Testing)

(ert-deftest arch-test-mock-cli-foreign-packages ()
  "Test foreign package detection with mocked pacman command."
  (cl-letf (((symbol-function 'arch--run-sync)
             (lambda (args &optional _req)
               (if (equal args '("pacman" "--query" "--foreign"))
                   "paru-bin 2.0.3-1\nemacs-git 31.0.50-1\n"
                 ""))))
    (let ((foreign (arch--foreign-packages)))
      (should (hash-table-p foreign))
      (should (gethash "paru-bin" foreign))
      (should (gethash "emacs-git" foreign))
      (should-not (gethash "bash" foreign)))))

(ert-deftest arch-test-mock-cli-upgradeable-packages ()
  "Test upgradeable package parsing with mocked pacman -Qu output."
  (cl-letf (((symbol-function 'arch--run-sync)
             (lambda (args &optional _req)
               (if (equal args '("pacman" "--query" "--upgrades"))
                   (concat "linux 6.8.1.arch1-1 -> 6.8.2.arch1-1\n"
                           "mesa 24.0.1-1 -> 24.0.2-1\n"
                           "warning: database file for 'core' does not exist\n")
                 ""))))
    (let ((upgradeable (arch--upgradeable-packages)))
      (should (hash-table-p upgradeable))
      (should (gethash "linux" upgradeable))
      (should (gethash "mesa" upgradeable))
      (should-not (gethash "warning:" upgradeable)))))

(ert-deftest arch-test-mock-cli-pacman-operations ()
  "Test pacman backend operations using mocked CLI sync output."
  (cl-letf (((symbol-function 'arch--run-sync)
             (lambda (args &optional _req)
               (cond
                ((equal (take 3 args) '("pacman" "--sync" "--search"))
                 "extra/git 2.44.0-1\n    the fast distributed version control system\n")
                ((equal (take 3 args) '("pacman" "--query" "--info"))
                 "Name : git\nVersion : 2.44.0-1\nDescription : Fast VCS\n")
                ((equal (take 3 args) '("pacman" "--query" "--list"))
                 "git /usr/\ngit /usr/bin/\ngit /usr/bin/git\ngit /usr/share/\ngit /usr/share/man/\n")
                (t "")))))
    (let ((search-results (arch--pacman-search "git")))
      (should (= (length search-results) 1))
      (should (equal (arch-pkg-name (car search-results)) "git")))
    (let ((info (arch--pacman-info "git")))
      (should (equal (plist-get info 'name) "git"))
      (should (equal (plist-get info 'version) "2.44.0-1")))
    (let ((files (arch--pacman-files "git")))
      (should (equal files '("/usr/bin/git"))))))

(provide 'test-arch)
;;; test-arch.el ends here
