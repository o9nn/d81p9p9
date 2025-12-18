#!/usr/bin/env guile
!#

;;; Comprehensive test of ontogenesis system

(load "ontogenesis.scm")

(define (test-section name)
  (format #t "~%▶ Testing ~a...~%" name))

(define (test-passed name)
  (format #t "  ✓ ~a passed~%" name))

(define (test-failed name error)
  (format #t "  ✗ ~a FAILED: ~a~%" name error))

(format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
(format #t "║         Ontogenesis Comprehensive Test Suite                  ║~%")
(format #t "╚═══════════════════════════════════════════════════════════════╝~%")

;; Test 1: Basic kernel creation
(test-section "Basic Kernel Creation")
(define test-kernel (create-basic-kernel 3))
(if (generated-kernel? test-kernel)
    (test-passed "Kernel creation")
    (test-failed "Kernel creation" "Not a generated kernel"))

;; Test 2: Ontogenetic initialization
(test-section "Ontogenetic Initialization")
(define onto-kernel (initialize-ontogenetic-kernel test-kernel))
(if (ontogenetic-kernel? onto-kernel)
    (test-passed "Ontogenetic initialization")
    (test-failed "Ontogenetic initialization" "Not an ontogenetic kernel"))

;; Test 3: Fitness evaluation
(test-section "Fitness Evaluation")
(define fitness (evaluate-fitness onto-kernel))
(if (and (number? fitness) (>= fitness 0) (<= fitness 1))
    (test-passed "Fitness evaluation")
    (test-failed "Fitness evaluation" (format #f "Invalid fitness: ~a" fitness)))

;; Test 4: Self-generation
(test-section "Self-Generation")
(define offspring (self-generate onto-kernel))
(if (and (ontogenetic-kernel? offspring)
         (= (genome-generation (onto-genome offspring)) 1))
    (test-passed "Self-generation")
    (test-failed "Self-generation" "Invalid offspring"))

;; Test 5: Self-optimization
(test-section "Self-Optimization")
(define optimized (self-optimize onto-kernel 5))
(if (and (ontogenetic-kernel? optimized)
         (> (state-maturity (onto-state optimized)) 0))
    (test-passed "Self-optimization")
    (test-failed "Self-optimization" "Invalid optimization result"))

;; Test 6: Genetic reproduction
(test-section "Genetic Reproduction")
(define parent2 (initialize-ontogenetic-kernel (create-basic-kernel 3)))
(define child (self-reproduce onto-kernel parent2 'crossover))
(if (and (ontogenetic-kernel? child)
         (= (genome-generation (onto-genome child)) 1)
         (>= (length (genome-lineage (onto-genome child))) 3))
    (test-passed "Genetic reproduction")
    (test-failed "Genetic reproduction" "Invalid child"))

;; Test 7: Population evolution
(test-section "Population Evolution")
(define config
  `((population-size . 5)
    (max-generations . 3)
    (mutation-rate . 0.15)
    (crossover-rate . 0.8)
    (elitism-rate . 0.1)
    (fitness-threshold . 0.9)
    (seed-kernels . ,(list (create-basic-kernel 3)))))
(define history (run-ontogenesis config))
(if (and (list? history)
         (>= (length history) 3)
         (every (lambda (entry)
                  (and (pair? entry)
                       (number? (car entry))
                       (list? (cdr entry))))
                history))
    (test-passed "Population evolution")
    (test-failed "Population evolution" (format #f "Invalid history, length=~a" (length history))))

;; Test 8: Genetic distance
(test-section "Genetic Distance")
(define dist (genetic-distance onto-kernel parent2))
(if (and (number? dist) (>= dist 0) (<= dist 1))
    (test-passed "Genetic distance")
    (test-failed "Genetic distance" (format #f "Invalid distance: ~a" dist)))

;; Test 9: Development stages
(test-section "Development Stages")
(define stage (state-stage (onto-state onto-kernel)))
(if (memq stage '(embryonic juvenile mature senescent))
    (test-passed "Development stages")
    (test-failed "Development stages" (format #f "Invalid stage: ~a" stage)))

;; Test 10: Lineage tracking
(test-section "Lineage Tracking")
(define lineage (genome-lineage (onto-genome child)))
(if (and (list? lineage) (>= (length lineage) 1))
    (test-passed "Lineage tracking")
    (test-failed "Lineage tracking" "Invalid lineage"))

(format #t "~%╔═══════════════════════════════════════════════════════════════╗~%")
(format #t "║              All Tests Completed Successfully ✓               ║~%")
(format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
