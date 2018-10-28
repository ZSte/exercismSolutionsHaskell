module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = C a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (x, y) = C x y

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (C x y) = C x (- y)

abs :: Floating a => Complex a -> a
abs (C x y) = sqrt (x * x + y * y)

real :: Num a => Complex a -> a
real (C x _) = x

imaginary :: Num a => Complex a -> a
imaginary (C _ y) = y

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (C x1 y1) (C x2 y2) = C (x1 * x2 - y1 * y2) (y1 * x2 + x1 * y2)

add :: Num a => Complex a -> Complex a -> Complex a
add (C x1 y1) (C x2 y2) = C (x1 + x2) (y1 + y2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub a (C x1 y1) = add a (C (- x1) (- y1))

div :: Fractional a => Complex a -> Complex a -> Complex a
div (C x1 y1) (C x2 y2) = C ((x1 * x2 + y1 * y2) / q) ((y1 * x2 - x1 * y2) / q)
    where q = x2 * x2 + y2 * y2
