module Deltas (
) where

import qualified Data.Bits as Bits

unary_detla =  [
                  -- logical unary operator over Bool
                  ( not_bool, not ),
                  -- bitwise unary operator over Nat
                  ( negate_nat, complement )
               ]

binary_delta = [  -- Arithmetic binary operators over Nat
                  ( plus_nat, (+) ),
                  ( minus_nat, (-) ),
                  ( mult_nat, (*) ),
                  ( div_nat, quot ),
                  ( mod_nat, rem ),
                  -- comparison binary operators over Nat
                  ( lt_nat, (<) ),
                  ( gt_nat, (>) ),
                  ( lteq_nat, (<=) ),
                  ( gteq_nat, (>=) ),
                  -- equality binary operators over Nat|Bool
                  ( eq_nat, (==) ),
                  ( neq_nat, (/=) ),
                  ( eq_bool, (==) ),
                  ( neq_bool, (/=) ),
                  -- logical binary operators over Bool
                  ( and_bool, (&&) ),
                  ( or_bool, (||) ),
                  -- bitwise binary operators over Nat
                  ( and_bitwise_nat, (.&.) ),
                  ( or_bitwise_nat, (.|.) ),
                  ( xor_bitwise_nat, xor ),
                  ( left_shift_nat, shiftL ),
                  ( right_shift_nat, shiftR )
               ]

deltas = 
   map insert 

un_op :: ( a -> a ) -> Term -> Term
un_op f term = 
   case term of 
      Cons (Prim name _ _ ) -> Cons ( Prim (show (f ) ) 0 [] )
      _ -> Error

bin_op :: (a -> a -> a) [Term] -> Term
bin_op f terms = 
   bin_op' f terms 
   where bin_op' f (term:terms) = 
            case term of
               Cons (Prim name _ _ ) -> f (read name) (bin_op' f terms)
               _ -> Error
         bin_op' f [] = 
