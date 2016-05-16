;;; CAVIART-VCGEN - A verification condition generator for the CAVI-ART project
;;; developed originally at GPD UCM.
;;; Copyright (C) 2016 Santiago Saavedra López, Grupo de Programación Declarativa -
;;; Universidad Complutense de Madrid
;;;
;;; This file is part of CAVIART-VCGEN.
;;;
;;; CAVIART-VCGEN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; CAVIART-VCGEN is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with CAVIART-VCGEN.  If not, see <http://www.gnu.org/licenses/>.

(declaim (optimize debug))
(in-package :ir.vc.builtins)

(deftype ir.vc.builtins:int () `(cl:integer ,cl:most-negative-fixnum ,cl:most-positive-fixnum))
(deftype ir.vc.builtins:bool () '(cl:member true false))

(macrolet ((boolean-external-op (opname)
	     `(pushnew '(,opname ((a int) (b int)) ((r int))
			 (declare (assertion
				   (precd true)
				   (postcd (@ = r (@ ,opname a b)))))
			 (error "Opaque term already verified."))
		       ir.vc.core:*external-functions*)))
  (boolean-external-op +)
  (boolean-external-op -)
  (boolean-external-op *)
  (boolean-external-op /)
  (boolean-external-op <)
  (boolean-external-op <=)
  (boolean-external-op =)
  (boolean-external-op >)
  (boolean-external-op >=))

(pushnew '(length ((a (array int))) ((r int))
	   (declare (assertion
		     (precd true)
		     (postcd (@ = r (@ length a)))))
	   (error "Opaque term already verified."))
	 ir.vc.core:*external-functions*)

(pushnew '(partition ((v (array int)) (l int) (r int)) ((vres (array int)) (split int))
	   (declare (assertion
		     (precd (and
			     (@ <= 0 l r)
			     (@ < r (@ length v))))
		     (postcd (and (@ permut_sub v vres l (@ + 1 r))
				  (@ <= l split r)
				  (:forall ((j1 int))
					   (-> (and (@ <= l j1)
						    (@ < j1 split))
					       (@ <= (@ get vres j1)
						     (@ get vres split))))
				  (:forall ((j2 int))
					   (-> (and (@ < split j2)
						    (@ <= j2 r))
					       (@ >= (@ get vres j2)
						     (@ get vres split))))))))
	   (error "Opaque term already verified."))
	 ir.vc.core:*external-functions*)
