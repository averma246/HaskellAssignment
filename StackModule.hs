 
 
------------------------------------------------------------------------------------------
                    -- Implementation of a stack -- 
                             -- ANA VERMA -- 
                           -- ALLAN J PHILLIPS--
------------------------------------------------------------------------------------------


module StackModule (initStack, pop, push, peek, isEmpty) where

data Stack a = EmptyStack | Top a (Stack a) 
    deriving Show
    
--initializing the stack does this
initStack v = Top v (EmptyStack)

--return the stack without the top
pop (EmptyStack) = EmptyStack
pop (Top v s) = s

--add to top of stack
push v (EmptyStack) = initStack v
push v1 (Top v s) = Top v1 (Top v s)

--look at first value without changing
peek (Top v s) = v

-- Checks if the stack is empty or not -- useful if checking whether calling peek would work
isEmpty (EmptyStack) = True
isEmpty (Top v s) = False
