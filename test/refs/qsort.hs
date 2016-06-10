--- CAVIART-VCGEN - A verification condition generator for the CAVI-ART project
--- developed originally at GPD UCM.
--- Copyright (C) 2016 Santiago Saavedra López, Grupo de Programación Declarativa -
--- Universidad Complutense de Madrid

--- This file is part of CAVIART-VCGEN.

--- CAVIART-VCGEN is free software: you can redistribute it and/or modify
--- it under the terms of the GNU Affero General Public License as
--- published by the Free Software Foundation, either version 3 of the
--- License, or (at your option) any later version.

--- CAVIART-VCGEN is distributed in the hope that it will be useful,
--- but WITHOUT ANY WARRANTY; without even the implied warranty of
--- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--- GNU Affero General Public License for more details.

--- You should have received a copy of the GNU Affero General Public License
--- along with CAVIART-VCGEN.  If not, see <http://www.gnu.org/licenses/>.


module ACME.Quicksort where

get :: [a] -> Int -> a
get (x:xs) 0 = x
get (x:xs) n | n > 0 = get xs (n - 1)

swap :: [a] -> Int -> Int -> [a]
swap l i j  | i < j = first ++ (get l j) : middle ++ (get l i) : last
  where
    first = take i l
    middle = take (j - i - 1) $ drop (i + 1) l
    last = drop (j + 1) l

swap l i j | i == j = l
swap l i j | i > j = swap l j i


k a i v m = if get a i < v then
              let m1 = m + 1 in
              let ares = swap a i m1 in
              (ares, m1)
            else
              (a, m)

loop v a l r i m = if i <= r - 1 then
                let (a1, m1) = k a i v m in
                let i1 = i + 1 in
                loop v a1 l r i1 m1
              else
                (a, m)

quick_rec a l r =
  if l + 1 < r then
    let v = get a l in
    let m = l in
    let (a1, m1) = loop v a l r (l + 1) l in
    let a2 = swap a1 l m1 in
    let a3 = quick_rec a2 l m1 in
    quick_rec a3 (m1 + 1) r
  else
    a

l = [9, 5, 7, 3, 1]

quicksort a = quick_rec a 0 (length a)
