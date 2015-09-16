(ns g-utility.jfx_main
  )

;(import '(javafx.application Application))
;(import '(javafx.stage Stage))
(import '(javafx.scene Scene))

(import '(javafx.scene.layout VBox))
(import '(javafx.scene.control Button))



(defn mkScene[]
  (let [
        box (VBox.)
        bt (Button. "Yessss")
        st (Scene. box 700 400)
        ]
    (.. box getChildren (add bt) )
    st
    )
  )
