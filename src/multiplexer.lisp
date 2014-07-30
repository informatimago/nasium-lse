
(defstruct virtual-machine
  name
  password
  model
  console-count
  file-system-root
  shared-file-system-root
  tape-root
  initial-root
  accounts)

(defstruct account
  name
  password
  default-id)

(defparameter *virtual-machines*
  (list (make-virtual-machine
         :name                    "MITRA-15"
         :password                nil
         :model                   :mitra-15
         :console-count           18
         :file-system-root        "/srv/lse/vm/mitra-15/files/"
         :shared-file-system-root "/srv/lse/vm/shared/files/"
         :tape-root               "/srv/lse/vm/shared/tapes/"
         :initial-root            "/srv/lse/vm/template/files/"
         :accounts                (list (make-account
                                         :name "default"
                                         :password nil
                                         :default-id 1)))
        (make-virtual-machine
         :name                    "T-1600"
         :password                nil
         :model                   :t-1600
         :console-count           18
         :file-system-root        "/srv/lse/vm/t-1600/files/"
         :shared-file-system-root "/srv/lse/vm/shared/files/"
         :tape-root               "/srv/lse/vm/shared/tapes/"
         :initial-root            "/srv/lse/vm/template/files/"
         :accounts                (list (make-account
                                         :name "default"
                                         :password nil
                                         :default-id 1)
                                        (make-account
                                         :name "system"
                                         :password "system"
                                         :default-id 10)))))

(defun create-virtual-machine (virtual-machine)
  
  )

(defun initialize-virtual-machine-paths (vm)

  )

(defun menu (items &key (title) (key (function identity)) (quit))
  (if (= 1 (length items))
      (first items)
      (loop
        (when title (io-format *task* "~2%~A~2%" title))
        (loop
          :for i :from 1
          :for item :in items
          :do (io-format *task* "~4D) ~A~%" i (funcall key item))
          :finally (when quit (io-format *task* "~4D) ~A~%" 0 quit))
                   (io-format *task* "~%Votre choix:"))
        (io-finish-output *task*)
        (let* ((choice (string-trim " " (io-read-line *task*)))
               (n (ignore-errors (parse-integer choice))))
          (cond
            ((eql 0 n)
             (return-from menu nil))
            ((and n (<= 1 n (length items)))
             (return-from menu (elt items (1- n))))
            (t
             (io-format *task* "~%Choix invalide.~%")))))))


(defun select-virtual-machine (machines)
  (menu machines
    :title "Liste des machines virtuelles"
    :key (function virtual-machine-name)
    :quit "- déconnecter."))


(defun password-hash (password) password)

(defun validate (account password)
  (or (not (account-password account))
      (equal (password-hash password)
             (account-password account))))

(defun select-account (accounts)
  (if (= 1 (length accounts))
      (first accounts)
      (loop
        (io-format *task* "Compte: ")
        (io-finish-output *task*)
        (let* ((name     (string-trim " " (io-read-line *task*)))
               (account  (find name accounts
                               :key (function account-name)
                               :test (function string=)))
               (password (if (or (not account)
                                 (account-password account))
                             (progn
                               (io-format *task* "Mot de passe: ")
                               (io-finish-output *task*)
                               (io-read-line *task* :echo nil))
                             (account-password account))))
          (if (validate account password)
              (return-from select-account account)
              (io-format *task* "~%Compte invalide.~%"))))))

(defun boot (vm account)
  (io-format *task* "~2%Booting ~A for ~A~2%"
          (virtual-machine-name vm)
          (account-name account))
  (io-finish-output *task*))


(defun multiplexer ()
  (let* ((vm      (select-virtual-machine *virtual-machines*))
         (account (progn
                    (io-format *task* "~2%Bienvenue sur l'émulateur LSE ~A~2%"
                               (virtual-machine-name vm))
                    (select-account (virtual-machine-accounts vm)))))
    (boot vm account)))


