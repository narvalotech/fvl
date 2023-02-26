;; play with dbus + bluetooth

(ql:quickload "dbus")

(dbus:with-open-bus (bus (dbus:session-server-addresses))
  (dbus:with-introspected-object (notifications bus "/org/freedesktop/Notifications" "org.freedesktop.Notifications")
    (notifications "org.freedesktop.Notifications" "Notify"
                   "Test" 0 "" "Test" "This is a test; I repeat, this is a test." nil nil -1)))

(defun make-bus ()
  (let (event-base connection bus)
    (setf event-base (make-instance 'iolib:event-base))
    (setf connection (dbus:open-connection event-base (dbus:system-server-addresses)))
    (dbus:authenticate (dbus:supported-authentication-mechanisms connection) connection)
    (setf bus (make-instance 'dbus:bus :name (dbus:hello connection) :connection connection))
    bus))

;; (iolib:set-io-handler event-base *standard-output* :write ())

(progn
  (defparameter *bus* (make-bus))
  (defparameter *con* (slot-value *bus* 'dbus/protocols:connection))
  (dbus:get-machine-id *bus*))

;; Get the full bluetooth object tree
(dbus:get-managed-objects *bus* "org.bluez" "/")

(dbus:get-property *bus* "org.bluez" "/org/bluez/hci0" "org.bluez.Adapter1" "Name")

(defparameter hash (slot-value obj 'dbus/introspect::interfaces))

(loop for k being the hash-key of hash do (print k))
(loop for k
      being the hash-key
      using (hash-value v) of hash
      do (format t "~a ~a~%" k v))

(maphash (lambda (key val)
           (format t "key: ~a val:~a~&" key val))
         hash)

(gethash "org.bluez.Adapter1" hash)
(dbus:interface-property "Name" (gethash "org.bluez.Adapter1" hash))
(dbus:list-interface-properties (gethash "org.bluez.Adapter1" hash))

(dbus:object-interface "org.bluez.Adapter1"
 (dbus:make-object-from-introspection *con* "/org/bluez/hci0" "org.bluez"))

;; doesn't work
;; doesn't help that this prop is read-only
(dbus/convenience:set-property *bus* "org.bluez" "/org/bluez/hci0" "org.bluez.Adapter1" "Name" "newname")

;; try to scan for devices:
(defparameter obj (dbus:make-object-from-introspection *con* "/org/bluez/hci0" "org.bluez"))
(defparameter obj (dbus:make-object-from-introspection *con* "/" "org.bluez"))
(dbus:object-invoke obj "org.bluez.Adapter1" "StartDiscovery")
(dbus:object-invoke obj "org.bluez.Adapter1" "StopDiscovery")

(dbus:define-dbus-object bluez-service (:path "/"))

(dbus:sigexp "oa{sa{sv}}")
;; ((:ARRAY (:DICT-ENTRY :STRING (:ARRAY (:DICT-ENTRY :STRING :VARIANT)))))
(dbus:define-dbus-signal-handler (bluez-service interfaces-added)
    ((object :OBJECT-PATH) (device (:ARRAY (:DICT-ENTRY :STRING (:ARRAY (:DICT-ENTRY :STRING :VARIANT))))))
  (:interface "org.freedesktop.DBus.ObjectManager")
  (format t "Got signal w arg ~A~%" device))

(dbus:add-match *bus* :type :signal :interface "org.bluez")

(trace dbus:receive-message-no-hang)
(trace dbus:drain-pending-messages)
(dbus:drain-pending-messages *con*)
(dbus:publish-objects *bus*)

(dbus:with-open-bus (bus (dbus:session-server-addresses))
  (dbus:add-match bus :type :signal :interface "org.bluez")
  (format t "Bus conn name: ~A~%" (dbus:bus-name bus))
  (dbus:publish-objects bus))

(loop for message = (dbus:receive-message-no-hang *con*)
           while message
           do (format t "message ~A~%" message))

;; can use qdbusviewer/d-feet (gui) or dbus-monitor

;; how to scan devices
;; - get HCI adapter (implements Adapter1 interface)
;; - get list of mgd obj implementing Device1 interface
;; - start scanning
;; - get delivered InterfacesAdded signals for each new device
;; - query the already-scanned devices with GetManagedObjects
;; - unreachable devices removed after (default) 30s
;;
;; When a property of a mgd device obj changes
;; -> PropertiesChanged signal emitted
;;
;; Start scanning: StartDiscovery
