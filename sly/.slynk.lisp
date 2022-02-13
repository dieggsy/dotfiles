(push '(*print-circle* . t) slynk:*slynk-pprint-bindings*)

(defmethod initialize-instance :after ((stream slynk-gray::sly-output-stream) &rest args)
  (declare (ignore args))
  #+allegro
  (setf (interactive-stream-p stream) t))


