(ns gitsnitch.report.json
  (:require [cheshire.core :as json]))

(defn render-json
  "Render data as a JSON string."
  [data]
  (json/generate-string data {:pretty true}))

(defn print-json [data]
  (println (render-json data)))
