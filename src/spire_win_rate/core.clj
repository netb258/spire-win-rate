(ns spire-win-rate.core
  (:require [clojure.java.io :as io])
  (:require [cheshire.core :as json])
  (:gen-class))

(def RUNS-DIR "D:\\Luc\\Slay The Spire V3\\runs\\")

(defn filter-runs
  "Takes a list of file objects and returns a new list containing only the file objects that contain a specified search string."
  [search-string list-of-file-objects]
  (->> list-of-file-objects
       (filter #(.isFile %)) ;; Ensure it's a file (not a directory)
       (filter #(-> % slurp (.contains search-string))))) ;; Check if file contains the string

(defn print-a9-win-rate!
  [runs-as-list-of-file-objects]
  (let [a9-runs (filter-runs "\"ascension_level\":9" runs-as-list-of-file-objects)
        a9-losses (filter-runs "\"killed_by\":" a9-runs)
        num-a9-runs (double (count a9-runs))
        num-a9-losses (double (count a9-losses))]
    (println "Total A9 Runs: " num-a9-runs)
    (println "WinRate: " (- 100.0 (* 100.0 (/ num-a9-losses num-a9-runs))))))

(defn is-win? [run-as-file-object]
  (and
    (.isFile run-as-file-object) ;; Ensure it's a file (not a directory)
    (not (.contains (slurp run-as-file-object) "\"killed_by\":"))))

(def IRONCLAD-RUNS (file-seq (io/file (str RUNS-DIR "IRONCLAD"))))
(def SILENT-RUNS (file-seq (io/file (str RUNS-DIR "THE_SILENT"))))
(def DEFECT-RUNS (file-seq (io/file (str RUNS-DIR "DEFECT"))))
(def WATCHER-RUNS (file-seq (io/file (str RUNS-DIR "WATCHER"))))
(def ALL-RUNS (concat IRONCLAD-RUNS SILENT-RUNS DEFECT-RUNS WATCHER-RUNS))

(defn sort-by-longest-winstreaks
  "Takes a flat list of File objects (spire runs) and sorts it by longest winstreak.
  Note that the sorted result contains only wins. Any losses will be removed.
  Also note, that the resulting list will not be flat. The wins will be grouped together.
  Like this: ((java.io.File '0x51cb8774') ;; This is a single win.
             (java.io.File '0x46499c98', java.io.File '0x2af3d0e9') ;; This is a streak of two.
             (java.io.File '0x42df9779', java.io.File '0x17a1bb72', java.io.File '0x1a5c7ed5') ...) ;; Streak of three.
  As you can see, the longest winstreak will be last."
  [runs-as-list-of-file-objects]
  (let [runs-only-files (filter #(.isFile %) runs-as-list-of-file-objects) ;; We want only files (not directories).
        runs-sorted-by-date (sort-by #(Long/parseLong (get (json/parse-string (slurp %)) "local_time")) runs-only-files)
        ;; At this point we should have a list of lists like this:
        ;; ((java.io.File '0x51cb8774')
        ;;  (java.io.File '0x46499c98', java.io.File '0x2af3d0e9')
        ;;  (java.io.File '0x42df9779', java.io.File '0x17a1bb72', java.io.File '0x1a5c7ed5') ...)
        ;; NOTE: that this is all wins grouped together, but the list also contains all the losses grouped together.
        ;; In a sense it is a list of all win-streaks AND lose-streaks.
        win-and-lose-streaks (partition-by is-win? runs-sorted-by-date)
        win-streaks (filter #(is-win? (first %)) win-and-lose-streaks)]
    (sort-by count win-streaks)))

(defn format-spire-date-string
  "Slay the Spire stores all dates in strings like this: '20250502172604'.
  Basically the above string is a compact version of this: 2025 - 05 02, 17:26:04.
  This function takes a Slay the Spire date string and returns a new string that is more readable."
  [spire-date-string]
  ;; If we take the above example '20250502172604',
  ;; then this (partition 4) would return ((\2 \0 \2 \5) (\0 \5 \0 \2) (\1 \7 \2 \6))
  ;; After that we just put them into year, month-and-day, hour-and-minute.
  (let [[year month-and-day hour-and-minute] (partition 4 spire-date-string)
        [month day] (partition 2 month-and-day) ;; Again taking the above example: month should be (\0 \5) and day should be (\0 \2).
        [hour minute] (partition 2 hour-and-minute) ;; Now hour should be (\1 \7) and minute should be (\2 \6).
        year-str (clojure.string/join year)
        month-str (clojure.string/join month)
        day-str (clojure.string/join day)
        hour-str (clojure.string/join hour)
        minute-str (clojure.string/join minute)]
    (str year-str " - " month-str " " day-str ", " hour-str ":" minute-str)))

(defn print-dates!
  "Takes a list of spire runs as file objects and prints their dates in a readable manner."
  [runs-as-list-of-file-objects]
  (let [files-as-json-parsed (map #(json/parse-string (slurp %)) runs-as-list-of-file-objects)
        list-only-dates (map #(get % "local_time") files-as-json-parsed)]
  (println (clojure.string/join "\n" (map format-spire-date-string list-only-dates)))))

(defn print-best-streak! [runs-as-list-of-file-objects]
  (let [best-streak (last (sort-by-longest-winstreaks runs-as-list-of-file-objects))]
    (println (str "The longest streak is: " (count best-streak)))
    (println "The streak happened on these dates:")
    (print-dates! best-streak)
    (println "")))

(defn is-rotating?
  "A list of runs is considered rotating if it contains all 4 characters (ironclad, silent, defect and watcher)."
  [runs-as-list-of-file-objects]
  (let [files-as-json-parsed (map #(json/parse-string (slurp %)) runs-as-list-of-file-objects)
        list-only-characters (map #(get % "character_chosen") files-as-json-parsed)]
    (=
     (into (hash-set) list-only-characters)
     #{"IRONCLAD" "THE_SILENT" "DEFECT" "WATCHER"})))

(defn -main [& args]
  (println "")
  (println "------- Combined win rate. -------")
  (print-a9-win-rate! ALL-RUNS)
  (println "")

  (let [longest-a9-streak (->> ALL-RUNS (filter-runs "\"ascension_level\":9") sort-by-longest-winstreaks (filter #(is-rotating? %)) last)]
    (println (str "The longest rotating A9 winstreak is: " (count longest-a9-streak)))
    (println "The streak happened on these dates:")
    (print-dates! longest-a9-streak)
    (println ""))

  (println "------- IRONCLAD win rate. -------")
  (print-a9-win-rate! IRONCLAD-RUNS)
  (println "")
  (print-best-streak! IRONCLAD-RUNS)

  (println "------- SILENT win rate. -------")
  (print-a9-win-rate! SILENT-RUNS)
  (println "")
  (print-best-streak! SILENT-RUNS)

  (println "------- DEFECT win rate. -------")
  (print-a9-win-rate! DEFECT-RUNS)
  (println "")
  (print-best-streak! DEFECT-RUNS)

  (println "------- WATCHER win rate. -------")
  (print-a9-win-rate! WATCHER-RUNS)
  (println "")
  (print-best-streak! WATCHER-RUNS))
