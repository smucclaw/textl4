module Syntax where

import Data.Text as T
import Data.List.NonEmpty as NE

{-
----------
  TODOs
----------
  * Add !a
  * Figure out where exactly to add Sequence. Not clear to me rn if it's best understood as an operator on clauses or on events or ...

-------------
  Resources
-------------

* "Contract Automata: An Operational View of Contracts Between Interactive Parties"

* https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell

SCL:
* https://remu.grammaticalframework.org/contracts/time2016/dist/SCL/Syntax.hs
* "SCL: A domain-specific language for normative texts with timing constraints"
-}


{- | A Contract := one or more clauses (if more than one, then it represents the *conjunction* of those clauses). 
The sense in which I'm using 'contract' is more like Camilieri's than the CA paper (which sometimes treats 'contract' as being synonymous with 'clause').
-}
newtype Contract = MkContract { clauses :: NonEmpty Clause }
  deriving newtype (Eq, Ord)
  deriving stock Show

contractToClauses :: Contract -> [Clause]
contractToClauses (MkContract clauses) = NE.toList clauses

--------------
--  Clause
--------------

{- | CL_rest.
        C := O⊥(a) | P(a) | F⊥(a) | C∧C | [β]C | ⊤ | ⊥

    CL_rest does not have ⊕ (exclusive or) between deontic operators.

     Conjunction has been factored out to Contract -- a conjunction of clauses should be represented as a sequence of them (wrapped in a Contract).
-}
data Clause = Must   Event   -- ^ Obligation
            | May    Event   -- ^ Permission
            -- TODO: Think about whether to add a construct tt allows user to specify that these are the *only* options available to the actor
            | Shant  Event   -- ^ Prohibition
            | If     Guard  Clause         -- ^ [β]C -- the contract C must be executed if action β is performed
            | Top                          -- ^ ⊤
            | Bottom                       -- ^ ⊥
  deriving (Eq, Ord, Show)

-----------------------
---  Actions
-----------------------

-- | The notion of actions that's used with norms. In CL_rest, modalities (O, P and F) are only applied to atomic actions (CA p.28)
type ActionForNorm = Action

{- | An Action, as I'm using it, does *not* include the agent / party.
Note that I differ from Camilieri's SCL on this: he uses "the term action to mean both agent and act together".

β := 0 | 1 | a | !a | β&β |β.β |β∗

  I omit the concurrency operator @&@ because
    (i) truly simultaneous actions are rare in the legal / regulation context
    (ii) it can be simulated with interleaving
    (iii) according to "A framework for conflict analysis of normative texts written
    in controlled natural language", it results in exponential blowup
  
  Useful note re Impossible and Skip, from "A framework for conflict analysis...":
    We omit from the CNL the two special actions 0 and 1 since they have no obvious equivalent in English. Although they have useful algebraic properties in the logic, they do not appear naturally in any real contracts. 
    A notable exception is the construction [1∗]C which means that the clause C must be enforced at any state. 
    For this purpose, we added the keyword `always` which can be used in front of any clause, which adds the condition [1∗] in the corresponding CL formula. 
    Similarly we did not include the Kleene star in our CNL, except for its use in relation to always.
-}
type Action = Text

data Guard = GDone Event
             -- ^ Party did Action. This corresponds to the `InfixOrPostfixActionApplication` construct in the Lam4 Langium grammar, and to Camilieri's `GDone Action`
           | GNot Guard
             -- ^ Negation of guard condition
           | GTrue 
           | GFalse
  deriving stock (Eq, Ord, Show)


-----------------------
---  Traces, Events
-----------------------

-- | A trace is a sequence of events
type Trace = [Event]

{- | Event aka 'state of affairs'.

"We assume that
  * each event is in fact a pair containing the event itself and who is the actor (subject, or performer of the event),

  * and that the set Names contains all the possible actors included in a contract (in bilateral contracts as we are having here, there usually be only two subjects, namely the two parties involved in the contract)" (CA p. 30)

"We also assume two projection functions giving the action itself and the subject""
-}
data Event = MkEvent { getActor :: Party
                     , getAction :: ActionForNorm }
  deriving stock (Eq, Ord, Show)


-----------------------
  ---  Party
-----------------------

newtype Party = MkParty { getName :: Text }
  deriving newtype (Eq, Ord)
  deriving stock (Show)
