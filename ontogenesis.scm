#!/usr/bin/env guile
!#

;;; ========================================================================
;;; ONTOGENESIS: Self-Generating, Evolving Kernels
;;; ========================================================================
;;;
;;; Implementation of self-generating kernels through recursive application
;;; of differential operators. Kernels can generate themselves, optimize
;;; themselves, reproduce with other kernels, and evolve across generations.
;;;
;;; Based on B-series expansion and A000081 tree enumeration.
;;;

(use-modules (srfi srfi-1)    ; List library
             (srfi srfi-9)    ; Record types
             (srfi srfi-19)   ; Time library
             (ice-9 format)   ; Formatting
             (ice-9 match))   ; Pattern matching

;; Load A000081 core implementation
(load "a000081.scm")

;;; ========================================================================
;;; Core Data Structures
;;; ========================================================================

;; Kernel Gene - Individual genetic unit
(define-record-type <kernel-gene>
  (make-kernel-gene type value mutable?)
  kernel-gene?
  (type gene-type)           ; 'coefficient, 'operator, 'symmetry, 'preservation
  (value gene-value set-gene-value!)
  (mutable? gene-mutable?))

;; Kernel Genome - Complete genetic information
(define-record-type <kernel-genome>
  (make-kernel-genome id generation lineage genes fitness age)
  kernel-genome?
  (id genome-id)
  (generation genome-generation set-genome-generation!)
  (lineage genome-lineage set-genome-lineage!)
  (genes genome-genes set-genome-genes!)
  (fitness genome-fitness set-genome-fitness!)
  (age genome-age set-genome-age!))

;; Ontogenetic State - Development stage information
(define-record-type <ontogenetic-state>
  (make-ontogenetic-state stage maturity events)
  ontogenetic-state?
  (stage state-stage set-state-stage!)       ; 'embryonic, 'juvenile, 'mature, 'senescent
  (maturity state-maturity set-state-maturity!)  ; 0.0 to 1.0
  (events state-events set-state-events!))  ; List of development events

;; Generated Kernel - B-series based kernel
(define-record-type <generated-kernel>
  (make-generated-kernel order coefficients operators grip)
  generated-kernel?
  (order kernel-order)
  (coefficients kernel-coefficients set-kernel-coefficients!)
  (operators kernel-operators set-kernel-operators!)
  (grip kernel-grip set-kernel-grip!))

;; Ontogenetic Kernel - Enhanced kernel with genetics
(define-record-type <ontogenetic-kernel>
  (make-ontogenetic-kernel kernel genome state)
  ontogenetic-kernel?
  (kernel onto-kernel)
  (genome onto-genome set-onto-genome!)
  (state onto-state set-onto-state!))

;;; ========================================================================
;;; Genetic Operations
;;; ========================================================================

;; Generate unique ID for genome
(define (generate-genome-id)
  (string-append "g-" 
    (number->string (random 1000000))
    "-"
    (number->string (get-internal-real-time))))

;; Create coefficient gene from value
(define (make-coefficient-gene value)
  (make-kernel-gene 'coefficient value #t))

;; Create operator gene
(define (make-operator-gene op-type)
  (make-kernel-gene 'operator op-type #t))

;; Create symmetry gene (immutable)
(define (make-symmetry-gene symmetry)
  (make-kernel-gene 'symmetry symmetry #f))

;; Create preservation gene (immutable)
(define (make-preservation-gene property)
  (make-kernel-gene 'preservation property #f))

;; Extract genes from generated kernel
(define (extract-genes kernel)
  (let* ((coeffs (kernel-coefficients kernel))
         (ops (kernel-operators kernel))
         (coeff-genes (map make-coefficient-gene coeffs))
         (op-genes (map make-operator-gene ops)))
    (append coeff-genes op-genes)))

;; Create genome from kernel
(define* (create-genome kernel #:key (parent-ids '()))
  (let* ((genes (extract-genes kernel))
         (id (generate-genome-id))
         (generation (if (null? parent-ids) 0 
                        (+ 1 (apply max (map (lambda (p) 
                                               (if (ontogenetic-kernel? p)
                                                   (genome-generation (onto-genome p))
                                                   0))
                                             parent-ids)))))
         (lineage (if (null? parent-ids) 
                     (list id)
                     (cons id (map (lambda (p)
                                     (if (ontogenetic-kernel? p)
                                         (genome-id (onto-genome p))
                                         "unknown"))
                                   parent-ids)))))
    (make-kernel-genome id generation lineage genes 0.0 0)))

;;; ========================================================================
;;; Kernel Initialization
;;; ========================================================================

;; Initialize ontogenetic kernel from generated kernel
(define (initialize-ontogenetic-kernel kernel)
  (let* ((genome (create-genome kernel))
         (state (make-ontogenetic-state 'embryonic 0.0 '())))
    (make-ontogenetic-kernel kernel genome state)))

;; Create basic generated kernel
(define (create-basic-kernel order)
  (let* ((num-coeffs (a000081-nth order))
         (coeffs (map (lambda (i) (* 0.1 (+ i 1))) 
                     (iota num-coeffs)))
         (ops (make-list order 'chain-rule)))
    (make-generated-kernel order coeffs ops 0.5)))

;;; ========================================================================
;;; Fitness Evaluation
;;; ========================================================================

;; Calculate grip quality (0.0 to 1.0)
(define (calculate-grip kernel)
  (let* ((coeffs (kernel-coefficients kernel))
         (order (kernel-order kernel))
         ;; Simple grip: based on coefficient stability
         (stability (/ 1.0 (+ 1.0 (apply + (map abs coeffs)))))
         ;; Coverage: based on number of coefficients
         (coverage (min 1.0 (/ (length coeffs) (* order 2))))
         ;; Efficiency: inverse of order
         (efficiency (/ 1.0 (+ 1.0 order))))
    (* 0.4 stability
       0.3 coverage
       0.3 efficiency)))

;; Calculate stability metric
(define (calculate-stability kernel)
  (let ((coeffs (kernel-coefficients kernel)))
    (if (null? coeffs) 
        0.0
        (/ 1.0 (+ 1.0 (apply max (map abs coeffs)))))))

;; Calculate efficiency metric
(define (calculate-efficiency kernel)
  (let ((order (kernel-order kernel)))
    (/ 1.0 (+ 1.0 order))))

;; Calculate novelty (genetic diversity)
(define (calculate-novelty kernel population)
  (if (null? population)
      0.5
      (let* ((distances (map (lambda (other)
                               (genetic-distance kernel other))
                            population))
             (avg-dist (/ (apply + distances) (length distances))))
        (min 1.0 avg-dist))))

;; Calculate symmetry preservation
(define (calculate-symmetry kernel)
  (let ((genes (genome-genes (onto-genome kernel))))
    (let ((symmetry-genes (filter (lambda (g) 
                                    (eq? (gene-type g) 'symmetry))
                                 genes)))
      (if (null? symmetry-genes)
          0.5
          1.0))))

;; Calculate overall fitness
(define* (evaluate-fitness ontok #:key (population '()))
  (let* ((kernel (onto-kernel ontok))
         (grip (calculate-grip kernel))
         (stability (calculate-stability kernel))
         (efficiency (calculate-efficiency kernel))
         (novelty (calculate-novelty ontok population))
         (symmetry (calculate-symmetry ontok))
         (fitness (+ (* 0.4 grip)
                    (* 0.2 stability)
                    (* 0.2 efficiency)
                    (* 0.1 novelty)
                    (* 0.1 symmetry))))
    (set-genome-fitness! (onto-genome ontok) fitness)
    fitness))

;; Genetic distance between two kernels
(define (genetic-distance kernel1 kernel2)
  (let* ((genes1 (genome-genes (onto-genome kernel1)))
         (genes2 (genome-genes (onto-genome kernel2)))
         (coeff-genes1 (filter (lambda (g) (eq? (gene-type g) 'coefficient)) genes1))
         (coeff-genes2 (filter (lambda (g) (eq? (gene-type g) 'coefficient)) genes2))
         (values1 (map gene-value coeff-genes1))
         (values2 (map gene-value coeff-genes2))
         (min-len (min (length values1) (length values2))))
    (if (= min-len 0)
        1.0
        (let* ((pairs (zip values1 values2))
               (diffs (map (lambda (p) (abs (- (car p) (cadr p)))) pairs))
               (avg-diff (/ (apply + diffs) min-len)))
          (min 1.0 avg-diff)))))

;;; ========================================================================
;;; Self-Generation
;;; ========================================================================

;; Apply chain rule for self-composition: (f∘f)' = f'(f(x)) · f'(x)
(define (apply-chain-rule coeffs)
  (let* ((n (length coeffs))
         (new-coeffs (map (lambda (i)
                           (let ((c (list-ref coeffs i)))
                             (* c c)))  ; Simplified chain rule
                         (iota n))))
    new-coeffs))

;; Self-generate: kernel generates offspring through recursive composition
(define (self-generate parent-kernel)
  (let* ((parent-gen-kernel (onto-kernel parent-kernel))
         (parent-genome (onto-genome parent-kernel))
         (parent-coeffs (kernel-coefficients parent-gen-kernel))
         (parent-order (kernel-order parent-gen-kernel))
         ;; Apply differential operator (chain rule)
         (new-coeffs (apply-chain-rule parent-coeffs))
         ;; Create offspring kernel
         (offspring-kernel (make-generated-kernel 
                            parent-order 
                            new-coeffs
                            (kernel-operators parent-gen-kernel)
                            (calculate-grip (make-generated-kernel parent-order new-coeffs '() 0.0))))
         ;; Create genome for offspring
         (offspring-genome (create-genome offspring-kernel #:parent-ids (list parent-kernel)))
         ;; Set initial state
         (offspring-state (make-ontogenetic-state 'embryonic 0.1 
                            (list (cons 'birth (get-internal-real-time))))))
    (make-ontogenetic-kernel offspring-kernel offspring-genome offspring-state)))

;;; ========================================================================
;;; Self-Optimization
;;; ========================================================================

;; Optimize a single coefficient
(define (optimize-coefficient coeff gradient learning-rate)
  (+ coeff (* learning-rate gradient)))

;; Self-optimize: kernel improves its own grip
(define (self-optimize ontok iterations)
  (let loop ((current ontok)
             (iter 0))
    (if (>= iter iterations)
        current
        (let* ((kernel (onto-kernel current))
               (coeffs (kernel-coefficients kernel))
               (order (kernel-order kernel))
               ;; Gradient ascent on grip
               (gradients (map (lambda (c) 
                               (if (> c 0) -0.01 0.01))  ; Simplified gradient
                             coeffs))
               (new-coeffs (map (lambda (c g) 
                                 (optimize-coefficient c g 0.1))
                               coeffs gradients))
               ;; Update kernel
               (new-kernel (make-generated-kernel order new-coeffs
                                                 (kernel-operators kernel)
                                                 (calculate-grip (make-generated-kernel order new-coeffs '() 0.0))))
               (new-onto (make-ontogenetic-kernel new-kernel
                                                 (onto-genome current)
                                                 (onto-state current))))
          ;; Update maturity
          (set-state-maturity! (onto-state new-onto)
                              (min 1.0 (+ (state-maturity (onto-state current))
                                        0.1)))
          ;; Record event
          (set-state-events! (onto-state new-onto)
                            (cons (cons 'optimization iter)
                                  (state-events (onto-state new-onto))))
          (loop new-onto (+ iter 1))))))

;;; ========================================================================
;;; Self-Reproduction
;;; ========================================================================

;; Crossover: single-point crossover of coefficient genes
(define (crossover-genes genes1 genes2)
  (let* ((coeff-genes1 (filter (lambda (g) (eq? (gene-type g) 'coefficient)) genes1))
         (coeff-genes2 (filter (lambda (g) (eq? (gene-type g) 'coefficient)) genes2))
         (len1 (length coeff-genes1))
         (len2 (length coeff-genes2))
         (min-len (min len1 len2)))
    (if (= min-len 0)
        genes1
        (let* ((point (random min-len))
               (offspring-coeffs (append (take coeff-genes1 point)
                                        (drop coeff-genes2 point)))
               ;; Keep non-mutable genes from parent1
               (other-genes (filter (lambda (g) 
                                     (not (eq? (gene-type g) 'coefficient)))
                                   genes1)))
          (append offspring-coeffs other-genes)))))

;; Mutate: random perturbation of mutable genes
(define (mutate-genes genes mutation-rate)
  (map (lambda (gene)
         (if (and (gene-mutable? gene)
                 (< (random:uniform) mutation-rate)
                 (eq? (gene-type gene) 'coefficient))
             (let* ((old-value (gene-value gene))
                    (perturbation (* (- (random:uniform) 0.5) 0.2))
                    (new-value (+ old-value perturbation)))
               (make-kernel-gene (gene-type gene) new-value (gene-mutable? gene)))
             gene))
       genes))

;; Extract coefficients from genes
(define (genes-to-coefficients genes)
  (map gene-value
       (filter (lambda (g) (eq? (gene-type g) 'coefficient)) genes)))

;; Self-reproduce: two kernels create offspring
(define* (self-reproduce parent1 parent2 method #:key (mutation-rate 0.1))
  (let* ((genes1 (genome-genes (onto-genome parent1)))
         (genes2 (genome-genes (onto-genome parent2)))
         (offspring-genes (case method
                           ((crossover) (crossover-genes genes1 genes2))
                           ((mutation) (mutate-genes genes1 mutation-rate))
                           ((cloning) genes1)
                           (else genes1)))
         ;; Apply mutation
         (mutated-genes (mutate-genes offspring-genes mutation-rate))
         ;; Create kernel from genes
         (coeffs (genes-to-coefficients mutated-genes))
         (order (kernel-order (onto-kernel parent1)))
         (operators (kernel-operators (onto-kernel parent1)))
         (offspring-kernel (make-generated-kernel order coeffs operators
                                                 (calculate-grip (make-generated-kernel order coeffs '() 0.0))))
         ;; Create genome
         (offspring-genome (create-genome offspring-kernel 
                                         #:parent-ids (list parent1 parent2)))
         (offspring-state (make-ontogenetic-state 'embryonic 0.0
                            (list (cons 'birth (get-internal-real-time))
                                  (cons 'method method)))))
    (make-ontogenetic-kernel offspring-kernel offspring-genome offspring-state)))

;;; ========================================================================
;;; Evolution Engine
;;; ========================================================================

;; Tournament selection
(define (tournament-selection population tournament-size)
  (let* ((candidates (take (shuffle population) 
                          (min tournament-size (length population))))
         (fitnesses (map (lambda (k) 
                          (genome-fitness (onto-genome k)))
                        candidates))
         (best-idx (list-index (lambda (f) 
                                (= f (apply max fitnesses)))
                              fitnesses)))
    (list-ref candidates best-idx)))

;; Shuffle list (Fisher-Yates)
(define (shuffle lst)
  (let ((vec (list->vector lst)))
    (let loop ((i (- (vector-length vec) 1)))
      (if (< i 1)
          (vector->list vec)
          (let* ((j (random (+ i 1)))
                 (temp (vector-ref vec i)))
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j temp)
            (loop (- i 1)))))))

;; Update development stages
(define (update-stages population)
  (map (lambda (kernel)
         (let* ((state (onto-state kernel))
                (genome (onto-genome kernel))
                (fitness (genome-fitness genome))
                (age (genome-age genome))
                (maturity (state-maturity state))
                (current-stage (state-stage state)))
           ;; Age kernel
           (set-genome-age! genome (+ age 1))
           ;; Update stage based on fitness and age
           (let ((new-stage (cond
                             ((and (eq? current-stage 'embryonic) (> maturity 0.3))
                              'juvenile)
                             ((and (eq? current-stage 'juvenile) (> fitness 0.6))
                              'mature)
                             ((and (eq? current-stage 'mature) (> age 10))
                              'senescent)
                             (else current-stage))))
             (set-state-stage! state new-stage))
           kernel))
       population))

;; Run evolution for one generation
(define (evolve-generation population config)
  (let* ((pop-size (length population))
         (mutation-rate (assoc-ref config 'mutation-rate))
         (crossover-rate (assoc-ref config 'crossover-rate))
         (elitism-rate (assoc-ref config 'elitism-rate))
         ;; Evaluate fitness
         (dummy (for-each (lambda (k) (evaluate-fitness k #:population population))
                         population))
         ;; Sort by fitness
         (sorted-pop (sort population 
                          (lambda (a b)
                            (> (genome-fitness (onto-genome a))
                               (genome-fitness (onto-genome b))))))
         ;; Keep elite individuals
         (elite-count (max 1 (inexact->exact (floor (* pop-size elitism-rate)))))
         (elites (take sorted-pop elite-count))
         ;; Generate offspring
         (offspring-count (- pop-size elite-count))
         (offspring (map (lambda (i)
                          (let ((parent1 (tournament-selection sorted-pop 3))
                                (parent2 (tournament-selection sorted-pop 3)))
                            (if (< (random:uniform) crossover-rate)
                                (self-reproduce parent1 parent2 'crossover 
                                              #:mutation-rate mutation-rate)
                                (self-generate parent1))))
                        (iota offspring-count)))
         ;; Combine elite and offspring
         (new-population (append elites offspring)))
    ;; Update development stages
    (update-stages new-population)))

;; Calculate population statistics
(define (population-stats population)
  (let* ((fitnesses (map (lambda (k) 
                          (genome-fitness (onto-genome k)))
                        population))
         (best-fitness (apply max fitnesses))
         (avg-fitness (/ (apply + fitnesses) (length fitnesses)))
         (diversity (/ (apply + (map (lambda (k1)
                                      (apply + (map (lambda (k2)
                                                     (genetic-distance k1 k2))
                                                   population)))
                                    population))
                      (* (length population) (length population)))))
    (list (cons 'best best-fitness)
          (cons 'average avg-fitness)
          (cons 'diversity diversity))))

;;; ========================================================================
;;; Main Evolution Runner
;;; ========================================================================

;; Run ontogenesis evolution
(define (run-ontogenesis config)
  (let* ((pop-size (assoc-ref config 'population-size))
         (max-generations (assoc-ref config 'max-generations))
         (fitness-threshold (assoc-ref config 'fitness-threshold))
         (seed-kernels (assoc-ref config 'seed-kernels))
         ;; Initialize population
         (initial-pop (append 
                       (map initialize-ontogenetic-kernel seed-kernels)
                       (map (lambda (i) 
                             (initialize-ontogenetic-kernel 
                              (create-basic-kernel (+ 2 (random 3)))))
                           (iota (- pop-size (length seed-kernels)))))))
    ;; Evolution loop
    (let loop ((generation 0)
               (population initial-pop)
               (history '()))
      (if (or (>= generation max-generations)
             (let ((stats (population-stats population)))
               (>= (assoc-ref stats 'best) fitness-threshold)))
          ;; Return history
          (reverse (cons (cons generation (population-stats population)) 
                        history))
          ;; Continue evolution
          (let* ((stats (population-stats population))
                 (new-pop (evolve-generation population config)))
            (loop (+ generation 1)
                  new-pop
                  (cons (cons generation stats) history)))))))

;;; ========================================================================
;;; Demonstration Functions
;;; ========================================================================

;; Print kernel information
(define (print-kernel-info ontok)
  (let* ((kernel (onto-kernel ontok))
         (genome (onto-genome ontok))
         (state (onto-state ontok)))
    (format #t "Kernel ~a~%" (genome-id genome))
    (format #t "  Generation: ~a~%" (genome-generation genome))
    (format #t "  Stage: ~a~%" (state-stage state))
    (format #t "  Fitness: ~,4f~%" (genome-fitness genome))
    (format #t "  Maturity: ~,4f~%" (state-maturity state))
    (format #t "  Coefficients: ~a~%" (kernel-coefficients kernel))))

;; Simple demonstration
(define (demo-simple)
  (format #t "~%=== Simple Self-Generation Demo ===~%~%")
  (let* ((base-kernel (create-basic-kernel 3))
         (parent (initialize-ontogenetic-kernel base-kernel))
         (offspring (self-generate parent)))
    (evaluate-fitness parent)
    (evaluate-fitness offspring)
    (format #t "Parent:~%")
    (print-kernel-info parent)
    (format #t "~%Offspring:~%")
    (print-kernel-info offspring)))

;; Evolution demonstration
(define (demo-evolution)
  (format #t "~%=== Evolution Demo ===~%~%")
  (let* ((config `((population-size . 10)
                   (max-generations . 20)
                   (mutation-rate . 0.15)
                   (crossover-rate . 0.8)
                   (elitism-rate . 0.1)
                   (fitness-threshold . 0.9)
                   (seed-kernels . ,(list (create-basic-kernel 3)
                                         (create-basic-kernel 4)))))
         (history (run-ontogenesis config)))
    (format #t "Evolution Results:~%")
    (for-each (lambda (entry)
               (let ((gen (car entry))
                     (stats (cdr entry)))
                 (format #t "Gen ~2d: Best=~,4f Avg=~,4f Diversity=~,4f~%"
                        gen
                        (assoc-ref stats 'best)
                        (assoc-ref stats 'average)
                        (assoc-ref stats 'diversity))))
             history)))

;;; ========================================================================
;;; Main Entry Point
;;; ========================================================================

(define (main)
  (format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║          👻 ONTOGENESIS: Self-Generating Kernels 👻           ║~%")
  (format #t "║                  B-Series Evolution System                     ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%")
  
  (demo-simple)
  (demo-evolution)
  
  (format #t "~%~%✨ Ontogenesis: Where mathematics becomes life ✨~%~%"))

;; Run if executed directly
(when (string=? (car (command-line)) "ontogenesis.scm")
  (main))
