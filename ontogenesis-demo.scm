#!/usr/bin/env guile
!#

;;; ========================================================================
;;; Ontogenesis Demo: Simple Self-Generation Example
;;; ========================================================================

(load "ontogenesis.scm")

(format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
(format #t "║    🧬 ONTOGENESIS: Self-Generating Kernels Demo 🧬            ║~%")
(format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")

;; Example 1: Simple Self-Generation
(format #t "═══ Example 1: Self-Generation ═══~%~%")
(format #t "Creating parent kernel...~%")
(define parent (initialize-ontogenetic-kernel (create-basic-kernel 3)))
(evaluate-fitness parent)

(format #t "Parent kernel:~%")
(format #t "  ID: ~a~%" (genome-id (onto-genome parent)))
(format #t "  Generation: ~a~%" (genome-generation (onto-genome parent)))
(format #t "  Fitness: ~,4f~%" (genome-fitness (onto-genome parent)))
(format #t "  Coefficients: ~a~%~%" (kernel-coefficients (onto-kernel parent)))

(format #t "Generating offspring through recursive composition...~%")
(define offspring (self-generate parent))
(evaluate-fitness offspring)

(format #t "Offspring kernel:~%")
(format #t "  ID: ~a~%" (genome-id (onto-genome offspring)))
(format #t "  Generation: ~a~%" (genome-generation (onto-genome offspring)))
(format #t "  Fitness: ~,4f~%" (genome-fitness (onto-genome offspring)))
(format #t "  Coefficients: ~a~%~%" (kernel-coefficients (onto-kernel offspring)))

;; Example 2: Multi-Generation Lineage
(format #t "~%═══ Example 2: Multi-Generation Lineage ═══~%~%")
(format #t "Tracing 5 generations of self-generation...~%~%")

(define lineage (list parent))
(let loop ((current offspring)
           (gen 2))
  (when (<= gen 5)
    (set! lineage (append lineage (list current)))
    (let ((next (self-generate current)))
      (evaluate-fitness next)
      (loop next (+ gen 1)))))

(format #t "Lineage:~%")
(for-each (lambda (k)
            (let ((genome (onto-genome k)))
              (format #t "  Gen ~a: fitness ~,4f, stage ~a~%"
                     (genome-generation genome)
                     (genome-fitness genome)
                     (state-stage (onto-state k)))))
          lineage)

;; Example 3: Self-Optimization
(format #t "~%═══ Example 3: Self-Optimization ═══~%~%")
(define unoptimized (initialize-ontogenetic-kernel (create-basic-kernel 4)))
(evaluate-fitness unoptimized)

(format #t "Before optimization:~%")
(format #t "  Fitness: ~,4f~%" (genome-fitness (onto-genome unoptimized)))
(format #t "  Maturity: ~,4f~%~%" (state-maturity (onto-state unoptimized)))

(format #t "Optimizing through 5 iterations...~%")
(define optimized (self-optimize unoptimized 5))
(evaluate-fitness optimized)

(format #t "After optimization:~%")
(format #t "  Fitness: ~,4f~%" (genome-fitness (onto-genome optimized)))
(format #t "  Maturity: ~,4f~%~%" (state-maturity (onto-state optimized)))

;; Example 4: Reproduction
(format #t "~%═══ Example 4: Genetic Reproduction ═══~%~%")
(define parent1 (initialize-ontogenetic-kernel (create-basic-kernel 3)))
(define parent2 (initialize-ontogenetic-kernel (create-basic-kernel 3)))
(evaluate-fitness parent1)
(evaluate-fitness parent2)

(format #t "Parent 1 fitness: ~,4f~%" (genome-fitness (onto-genome parent1)))
(format #t "Parent 2 fitness: ~,4f~%~%" (genome-fitness (onto-genome parent2)))

(format #t "Creating offspring via crossover...~%")
(define child (self-reproduce parent1 parent2 'crossover #:mutation-rate 0.1))
(evaluate-fitness child)

(format #t "Child fitness: ~,4f~%" (genome-fitness (onto-genome child)))
(format #t "Child generation: ~a~%" (genome-generation (onto-genome child)))
(format #t "Child lineage length: ~a~%~%" 
        (length (genome-lineage (onto-genome child))))

;; Example 5: Small Evolution
(format #t "~%═══ Example 5: Population Evolution ═══~%~%")
(format #t "Evolving population of 10 kernels over 10 generations...~%~%")

(define config
  `((population-size . 10)
    (max-generations . 10)
    (mutation-rate . 0.15)
    (crossover-rate . 0.8)
    (elitism-rate . 0.1)
    (fitness-threshold . 0.9)
    (seed-kernels . ,(list (create-basic-kernel 3)
                          (create-basic-kernel 4)))))

(define history (run-ontogenesis config))

(format #t "Evolution Results:~%")
(format #t "Gen  Best    Average  Diversity~%")
(format #t "───  ──────  ───────  ─────────~%")
(for-each (lambda (entry)
            (let ((gen (car entry))
                  (stats (cdr entry)))
              (format #t "~2d   ~,4f  ~,4f   ~,4f~%"
                     gen
                     (assoc-ref stats 'best)
                     (assoc-ref stats 'average)
                     (assoc-ref stats 'diversity))))
          history)

(format #t "~%✨ Ontogenesis: Where mathematics becomes life ✨~%~%")
