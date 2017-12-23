doc/                                                                                                0000775 0001750 0001750 00000000000 13215763513 011711  5                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               doc/ICFP2016/                                                                                       0000775 0001750 0001750 00000000000 13215763513 012743  5                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               doc/ICFP2016/fig-kinding.tex                                                                        0000664 0001750 0001750 00000004054 13216017506 015651  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[t]
  \begin{gather*}
    \frac{}{\Delta \vdash \skipk :: \kinds^\Unrestricted } \quad
    \frac{}{\Delta \vdash \,!B :: \kinds^\Linear} \quad \frac{}{\Delta
      \vdash \,?B :: \kinds^\Linear}
    \\
    \frac{\Delta \vdash T_1 :: \kinds^{m_1} \quad \Delta \vdash T_2 ::
      \kinds^{m_2}}{\Delta \vdash (T_1;T_2) :: \kinds^{\max
        (m_1,m_2)}}
    \\
    \frac{(\forall i\in I)~\Delta \vdash T_i :: \kinds^\Linear}{
      \Delta \vdash \oplus\{l_i\colon T_i\}_{i\in I} ::
      \kinds^\Linear}
    \quad
    \frac{(\forall i\in I)~\Delta \vdash T_i
      :: \kinds^\Linear}{ \Delta \vdash \&\{l_i\colon T_i\}_{i\in I} :: \kinds^\Linear}
    \\
    \frac{}{\Delta, x :: \kind \vdash x :: \kind} \quad
    \frac{}{\Delta, \alpha :: \kind \vdash \alpha :: \kind} \quad
    \frac{}{\Delta \vdash B :: \kindt^\Unrestricted}
    \\
    \frac{
      \Delta \Contr T%:\gamma 
      \quad
      \Delta, x :: \kind \vdash T :: \kind \quad
      \kind \le \kindt^\Linear
    }{\Delta \vdash \mu x.T :: \kind}
    \\
    \frac{
      \Delta \vdash T_1 :: \kindt^\Linear \quad
      \Delta \vdash T_2 :: \kindt^\Linear}{
      \Delta \vdash T_1\to T_2 :: \kindt^\Unrestricted}
    \quad
    \frac{
      \Delta \vdash T_1 :: \kindt^\Linear \quad
      \Delta \vdash T_2 :: \kindt^\Linear}{
      \Delta \vdash T_1\multimap T_2 :: \kindt^\Linear}
    \\
    \frac{
      \Delta \vdash T_1 :: \kindt^\Linear \quad
      \Delta \vdash T_2 :: \kindt^\Linear}{
      \Delta \vdash T_1\otimes T_2 :: \kindt^\Linear} \quad
    \frac{(\forall i\in I)~\Delta \vdash T_i :: \kindt^m}{ \Delta
      \vdash [l_i\colon T_i]_{i\in I} :: \kindt^m}
    \\
    \frac{\Delta \vdash T :: \kind_1 \quad \kind_1 \le \kind_2}{\Delta \vdash T :: \kind_2}
    \quad
    \frac{
      \Delta, \alpha :: \kind \vdash T :: \kindsch^m \quad
      \kind \le \kindt^\Linear}{
      \Delta \vdash \forall\alpha :: \kind.T :: \kindsch^m}
  \end{gather*}

  \caption{Kinding system, $\Delta \vdash T :: \kind$}
  \label{fig:kinding}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    doc/ICFP2016/treedata.cfs                                                                           0000664 0001750 0001750 00000000060 13216017506 015220  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               type Tree = Leaf
          | Node int Tree Tree
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                doc/ICFP2016/examples.tex                                                                           0000664 0001750 0001750 00000027375 13216017506 015314  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Context-Free Session Types in Action}
\label{sec:context-free-session}

To understand the requirements for the metatheory of
context-free session types, we first examine the type derivation of
\lstinline|sendTree| in Listing~\ref{listing:serializing}. Then we
turn to further examples that underline the expressiveness and the
usefulness of context-free session types.

\subsection{Sending Leaves}
\label{sec:sending-leaves}


To typecheck the first alternative of the \lstinline|sendTree|
function for sending leaves, we need to derive type \lstinline|alpha| for the code
fragment
\begin{lstlisting}
  select Leaf c
\end{lstlisting}
given that \lstinline{c : TreeChannel;alpha}. Anticipating the formal
definition in Section~\ref{sec:processes} (Figure~\ref{fig:typing}),
we sketch an informal typing rule for \lstinline|select|, which is
taken verbatim from GV~\cite{DBLP:journals/jfp/GayV10}:
\begin{equation}\label{eq:2}
    \frac{
      \vdash e : \oplus\{l_i\colon S_i\}_{i\in I}
      \quad
      j\in I
      % \quad
      % \cdot\vdash T_i :: \kinds^m
    }{
      \vdash \select {l_j} e \colon S_j
    }
\end{equation}
 The \lstinline|select|
operation expects a branch type $\oplus\{l_i\colon S_i\}$, but we are given the recursive
\lstinline|TreeChannel| type, which has to be unfolded first. Such
unfolding is to be expected in the 
presence of recursive types. As unfolding is not indicated in the term,
we require an \emph{equi-recursive treatment of recursion in types}~\cite{Pierce2002-tpl}.


After unfolding, we obtain
\begin{lstlisting}
  c : oplus{Leaf: skip,
        Node: !int;TreeChannel;TreeChannel};alpha
\end{lstlisting}
This type, a sequence of protocols, is still not in the form expected
by \lstinline|select|. Hence, we further
need to enrich type equivalence to enable us to \emph{commute the
continuation type \lstinline|alpha| inside the branches}.

After commutation, we obtain the typing
\begin{lstlisting}
  c : oplus{Leaf: skip;alpha,
        Node: !int;TreeChannel;TreeChannel;alpha}
\end{lstlisting}
which is finally in a form acceptable to \lstinline|select|. Applying
the typing rule~\eqref{eq:2} yields
\begin{lstlisting}
  select Leaf c : skip;alpha
\end{lstlisting}
At this point, we need to apply the \emph{monoid identity law} (which
also needs to be part of type equivalence) to obtain the desired
outcome.
\begin{lstlisting}
  select Leaf c : alpha
\end{lstlisting}


\subsection{Sending Nodes}
\label{sec:sending-nodes}

We turn to typechecking the second alternative of the
\lstinline|sendTree| function
\begin{lstlisting}
  let c1 = select Node c
      c2 = send x c1
      c3 = sendTree l c2
      c4 = sendTree r c3
  in  c4
\end{lstlisting}
given that 
\begin{lstlisting}
  x : int, l : Tree, r : Tree, c : TreeChannel
\end{lstlisting}

Typechecking the \lstinline|select| operation requires the same steps
as for leaves. We skip over those and note the resulting typing for
\lstinline|c1|.
\begin{lstlisting}
  c1 : !int;TreeChannel;TreeChannel;alpha
\end{lstlisting}
The \lstinline|send| operation just peels off the leading
\lstinline{!int} type, but our typing for \lstinline|c1| glosses over
an important detail, namely the bracketing of the $\scCompose\_\_$
operator. After commuting \lstinline|alpha| inside the branch type and
applying the \lstinline|select| rule, we are actually left with this
type:
\begin{lstlisting}
  c1 : (!int;(TreeChannel;TreeChannel));alpha
\end{lstlisting}
Again, we need to appeal to type equivalence to reassociate the
nesting of the sequence operator, that is, to apply the \emph{monoidal
associativity law}. The resulting type
\begin{lstlisting}
  c1 : !int;((TreeChannel;TreeChannel);alpha)
\end{lstlisting}
is compatible with the typing for \lstinline|send| and we can
proceed with
\begin{lstlisting}
  c2 : (TreeChannel;TreeChannel);alpha
\end{lstlisting}
Again, we need to reassociate:
\begin{lstlisting}
  c2 : TreeChannel;(TreeChannel;alpha)
\end{lstlisting}
At this point, we see the need for \emph{polymorphic recursion}: the
recursive call \lstinline|sendTree l c2| of
\begin{lstlisting}
sendTree : forallbeta.Tree -> TreeChannel;beta -> beta
\end{lstlisting}
must instantiate the type variable \lstinline|beta| to
\lstinline{(TreeChannel;alpha)}. With this instantiation, we obtain
\begin{lstlisting}
  c3 : TreeChannel;alpha
\end{lstlisting}
The second recursive call instantiates \lstinline|beta| to
\lstinline|alpha| (it could be treated monomorphically) and we readily
obtain the desired final outcome, which is equivalent to the outcome
of the first alternative:
\begin{lstlisting}
  c4 : alpha
\end{lstlisting}
In summary, the type system for context-free session types requires
polymorphism with polymorphic recursion.\footnote{This particular example can
be made to work without polymorphic recursion by abstracting the
recursive calls to \lstinline|transform| in a separate function with a
specialized type. However, we argue that it is advantageous to be able
to type the straightforward code that we present.} Furthermore, it relies on a
nontrivial notion of type equivalence that includes unfolding of
equi-recursive types, distributivity of branching over sequencing, and the
monoidal structure of \lstinline|skip| and sequencing (identity and
associativity laws). Our technical treatment of type equivalence in
Sections~\ref{sec:bisimulation} and~\ref{sec:decidability}
relies on a terminating unraveling operation that normalizes the
``head'' of a session type with respect to these notions. 

\subsection{Structure-Preserving Tree Transformation}
\label{sec:remote-tree-transf}

As another example for the expressiveness of context-free session
types, we present client and server code for a remote structure-preserving tree
transformation in Listing~\ref{listing:remote-tree-transformation}. It is based on the same tree datatype as before, but
it introduces a new channel type \lstinline|XformChan| that
receives the transformed node value after sending the old value and
the two subtrees. This code makes use of the \lstinline|receive|
operation that takes a channel of type \lstinline|!int;alpha| and
returns a linear pair of type \lstinline|intotimesalpha|. The pair
must be linear because channels in session-type calculi generally have
linear types to cater for the change of their type at each operation.
\lstinputlisting[float={t},captionpos={b},caption={Remote tree transformation},label={listing:remote-tree-transformation}]{tree.cfs}

The server function \lstinline|transform| demonstrates the use of
\lstinline|receive|. It also uses pattern matching to deconstruct the
linear pairs returned by recursive calls and by receiving integers. No
new issues arise in typing this function compared to
\lstinline|sendTree|. 

The function \lstinline|treeSum| is a suitable client for transformer
channels. It computes the accumulated sum at each tree node, so that
running \lstinline|transform| and
\lstinline|treeSum| concurrently results in a tree where each node value is
replaced by the sum of all node values below. The function
\lstinline|treeSum| takes an argument channel of type
\lstinline|dualof XformChan; alpha| where the \lstinline|dualof|
operator swaps sending and receiving types as usual. The
\lstinline|case| expression is the receiving counterpart of the
\lstinline|select| expression. It receives a label from a channel and
dispatches according to this label. Each branch of the
\lstinline|case| is a function that takes the respective continuation
of the channel and continues the interaction on that channel.

The final definition of \lstinline|go| stitches it all together. Using
\lstinline{new XformChan} it creates a new pair of channels, the types
of which are \lstinline{XFormChan} and its dual, it forks a new
process that runs \lstinline{treeSum} on the server channel, and
finally runs \lstinline{transform} on an example tree and the client channel.

The example also illustrates how channels are
closed. \lstinline|treeSum| (\lstinline|transform|) returns a
\emph{linear pair} of the accumulated sum (transformed tree) and a
depleted channel of type \lstinline|skip|. The function
\lstinline|fst| eliminates the linear pair and returns its first
component, which is possible because the second component is of an
unrestricted type (\lstinline|skip|). It implicitly closes the channel
by discarding it, as the channel end of type \lstinline|skip| can no longer be used for
interaction.

% which is possible because \lstinline|skip| is no
% longer restricted to be linear.

\subsection{Expression Server}
\label{sec:expression-server}

An example that is quite often used in the literature on session types
is an arithmetic server with a type like the one indicated in the
introduction:
\begin{displaymath}
  \sRecvChoice{
    \cgAlt{\mathit{add}}{\sRecv{\intk}\sRecv{\intk}\sSend{\intk}\End}
    ,
    \cgAlt{\mathit{neg}}{\sRecv{\intk}\sSend{\intk}\End}
  }
\end{displaymath}

\lstinputlisting[float={t},captionpos={b},caption={Arithmetic
  expression
  server},label={listing:arithmetic-expression-server}]{arithmetic-server.cfs}
 
Exploiting context-free session types, we can extend the scope of such
a server to receive and process arbitrary well-formed arithmetic
expressions. As an example consider the arithmetic expression server
for terms composed of constants, addition, and multiplication in
Listing~\ref {listing:arithmetic-expression-server}.
%
The implementation of the protocol is straightforward using the
techniques already described.

It is possible to extend this protocol to lazily traverse the term
(Listing~\ref{listing:lazy}). In this case, the server requests from
the client the parts of the term needed to complete the
evaluation. For instance, if a factor in a multiplication is zero, the
server can avoid to even ask for sending the other factor. We
elucidate this idea with a simplified protocol to explore a binary
tree lazily. No new features are required for its realization.

\lstinputlisting[deletekeywords={Left,Right},float={t},captionpos={b},caption={Lazy
tree traversal},label={listing:lazy}]{exploration.cfs}

A client connecting to the server \lstinline|exploreTree| first
connects to the root node of a tree. First it must check whether the
current node is a \lstinline|Leaf| or a \lstinline|Node|. If it is a
\lstinline|Node|, it can further explore the contents: it can ask for
the value or traverse the left subtree or the right subtree as often
as desired. Finally the client sends \lstinline|Exit| to return to the
parent node. 

The type describing this interaction is mutually recursive. The
``inner loop'' described by \lstinline|XploreNodeChan| is
tail-recursive like a regular session type, but the ``outer loop''
corresponding to \lstinline|XploreTreeChan| is not as its invocations
are intertwined with the inner loop.

% Now with a datatype to simplify the client's life.

% \lstinputlisting{arithmetic-server-data.cfs}


% Let's try a SePi-like version. All we need are context-free session
% types and type schemas\footnote{Predicative Polymorphism in
%   pi-Calculus. Vasco Thudichum Vasconcelos. In 6th Parallel
%   Architectures and Languages Europe, volume 817 of LNCS, pages
%   425--437. Springer, 1994.}. We witness the extra ``plumbing''
% typical of the pi-calculus, but otherwise very little (type only, in
% fact) extra basic notions.

% \lstinputlisting{arithmetic-server-sepi.cfs}


% The code for deserialization is analogous to the one for serialization and it can also be typed with
% the type structure explained in the introduction.
% \begin{alltt}
% recvT : Channel -> Tree \(\otimes\) Channel
% recvT c =
%   case c of
%   | Leaf : lambda c0.
%       let (n, c1) = receive (c0) in
%       (Leaf n, c1)
%   | Node : lambda c0.
%       let (r, c1) = recvT c0 in
%       let (l, c2) = recvT c1 in
%       (Node (l, r), c2)
% \end{alltt}


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                   doc/ICFP2016/fig-lts.tex                                                                            0000664 0001750 0001750 00000001363 13216017506 015030  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[tp]
  \begin{gather*}
    \DONE{\skipk}
    \qquad
    \frac{\DONE{S_1} \quad \DONE{S_2}}{\DONE{S_1; S_2}}
    \qquad
%    \frac{\DONE{S}}{\DONE{\mu x.S}}
    \frac{\DONE{S[\mu x.S/x]}}{\DONE{\mu x.S}}
    \\
    {A \LTSderives[A] \skipk }
    \qquad
    {\star\{l_i\colon S_i\}_{i\in I} \LTSderives[\star l_j] S_j}
    \\
    \frac{S_1 \LTSderives S_1'}{S_1; S_2 \LTSderives S_1';S_2}
    \quad\;\;
    \frac{\DONE{S_1} \quad S_2 \LTSderives S_2'}{S_1; S_2 \LTSderives S_2'}
    \quad\;\;
    \frac{S[\mu x.S/x] \LTSderives S'}{\mu x.S \LTSderives S'}
  \end{gather*}
  \caption{Labelled transition system for context free session types}
  \label{fig:lts}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                             doc/ICFP2016/fig-reduction.tex                                                                      0000664 0001750 0001750 00000002421 13216017506 016216  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[t]
  \begin{gather*}
    % FUNCTIONAL
    (\lambda a.e)v \reduces e\subs va
    \qquad
    \letin{a,b}{(u,v)}{e} \reduces e\subs ua\subs vb
    \\
    \match{(\ink\,l_j\,v)}{l_i\rightarrow e_i}  \reduces e_jv
    \qquad
    \fix ae \reduces e \subs{\fix ae}{a}
    \\
    \frac{e_1 \reduces e_2}{E[e_1] \reduces E[e_2]}
    \qquad
    E[\fork e] \reduces E[()] \PAR e
    \\
    E[\newk] \reduces (\nu a,b)E[(a,b)]
    \\ % PROCESSES
    (\nu a,b)(E_1[\send va] \PAR E_2[\recv b])
    \reduces
    (\nu a,b)(E_1[a] \PAR E_2[(v,b)])
    \\
    (\nu a,b)(E_1[\selectk\,l_j\,a] \PAR E_2[\casek\,b\,\ofk\,\{l_i\rightarrow e_i\}])
    \reduces  \qquad \qquad  \qquad\\
    \qquad \qquad \qquad \qquad \qquad \qquad \qquad \qquad (\nu a,b)(E_1[a] \PAR E_2[e_jb])
    \\
    \frac{p \reduces p'}{p\PAR q \reduces p'\PAR q}
    \quad
    \frac{p \reduces p'}{(\new a,b)p \reduces (\new a,b)p'}
    \quad
    \frac{p \equiv q\quad q \reduces q'}{p \reduces q'}
  \end{gather*}
  Context $E_1$ (resp.~$E_2$, resp.~$E$) does not bind~$a$ (resp.~$b$,
  resp.~$a$ and~$b$).
  \\
  Dual $(\nu b,a)$ rules for $\sendk/\recvk$ and $\selectk/\casek$
  omitted.
  \caption{Reduction relation}
  \label{fig:reduction}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                               doc/ICFP2016/popl16-reviews.txt                                                                     0000664 0001750 0001750 00000037023 13216017506 016307  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               ===========================================================================
                          POPL 2016 Review #111A
---------------------------------------------------------------------------
              Paper #111: Towards Context-Free Session Types
---------------------------------------------------------------------------

                         ===== Paper summary =====

Recursive definition of session types describe regular patterns of
interactions between processes, seen here in the technical sense of
"regular languages".  This can be limiting if the pattern of
interactions is more general and corresponds, for example, to
context-free languages.  In order to allow such session types, the
authors add two new session constructors: an empty session and a
general sequential composition of two sessions.  In the presence of
session polymorphism, these now allow some natural context-free
sessions to be typed.  Unfortunately, they also create a number of
technical complications, specifically a more complex notion of type
equality and a difficult type checking problem (which is actually left
to future work).

The key application domain of context-free session types is the
serialized form of tree-structured data types.  Interaction tends to
be in sequences or streams of data even if their natural structure is
that of a tree.  Such sequences can of course be represented with
regular session types already, but they cannot be captured precisely:
some illegal sequences of interactions would have to be detected
dynamically.

===== Comments for authors and suggestions to make this a strong paper =====

I found the paper to be quite intriguing.  Systems of session types
proposed in the literature are already so rich in expressive power
that it never occurred to me to consider this particular limitation.
There is a particular self-imposed restriction in this development,
namely to what they call "first-order" session types that I find
curious.  It seems that if we drop this restriction it should actually
be relatively easy to express the desired functionality.  I'll
elaborate on this in the following by providing three alternatives, one
of them in some detail.  Since these alternatives are available, it
would help me a great deal to have a clearer motivation for the
particular restriction (to first-order session types) and
generalization (to context-free sessions) considered together.  Since
the paper is otherwise well done and makes several contribution,
my judgment may ultimately depend on this point.

I am providing here a solution in SILL (Toninho et al, ESOP 2013),
which, incidentally, the author(s) seem to miss among the related
work.  Channel names are preceded by a quote.

type Tree = Leaf | Node Int Tree Tree;;

ltype TreeChan = +{ leaf : 1 ; node : Int /\ TreeChan * TreeChan * 1 };;

sendTree : Tree -> {TreeChan};;

'c <- sendTree t = case t of
| Leaf -> 'c.leaf ; close 'c
| Node x l r -> 'c.node ;
                send 'c x ;
                'd <- sendTree l ; send 'c 'd ;
                'e <- sendTree r ; send 'c 'e ;
                close 'c
;;

The point here is to use the tensor (written as '*' above) to create
tree-structured session that mimics the tree structure of the Tree
type.  There is also no difficulty in writing the corresponding
recvTree function.  The idiom '_ * 1' in the type is only used here
to obtain simpler code and could be dropped.  The treeSum example
can be rewrittin in an analogous way on this type.

No doubt the authors are aware of a second alternative, where the
session type over-approximates the legal communications.  For example,

ltype LinTree = +{ leaf : LinTree ; data : Int /\ LinTree ; done : 1 };;

While this is operationally adequate, it would not rule out incomplete
serializations of trees to be sent---these would have to be discovered
dynamically.

Finally, a third alternative would be use a dependently typed session
framework, such as as (Toninho et al., PPDP 2011).  Clearly, in this
framework the context-free refinements of the regular types can also
be described.  The cost of such an encoding in terms of proofs and
proof objects is not immediately clear, but I think it is at least
worth mentioning.

Given these alternatives, is it still worth considering a direct
extension to handle context-free session types?  The case can be made
for two reasons: (1) The code, for example, in Listing 1 is
exceptionally straightforward while the code shown above may be
considered slightly more complicated, and (2) sequentially composing
two session is a natural extension so it is worth investigating.
Unfortunately, it does not turn out to be too simple because of the
"commuting conversion" that have to be considered in the type
equality.  This, together with the restriction to first-order types
and the required polymorphic recursion, makes a concrete realization
somewhat doubtful while the solutions I sketched above are rather
straightforward from an implementation perspective.

Otherwise, the paper is competently done to the extent that I have
checked.  It is an interesting contribution of the paper that in the
first-order case, precise characterization of the trace sets in terms
of omega-context-free languages can be given.  This is technically
significant and culminates in the decidability of equivalence of
session types.  Ideally, this would be in the core of a type-checking
algorithm, but, as the authors note in the conclusion, they are
looking for sound approximations since the full algorithm may not be
practical.

As a side issue, give the Curry-Howard foundation for session types
given by Caires et al [3, and some refs above] and Wadler [19] one
might reasonably ask whether skip and composition have a logical
meaning.  This might provide some hints about a stronger system where
typing can also guarantee deadlock-freedom (which is not the case
here).  In fact, this might be considered another significant drawback
of the present system in light of these more recent discoveries since
three of the four alternative solutions sketched above also rule out
deadlocks statically.

===========================================================================
                          POPL 2016 Review #111B
---------------------------------------------------------------------------
              Paper #111: Towards Context-Free Session Types
---------------------------------------------------------------------------

                         ===== Paper summary =====

Traditional session types allow the expression of communication over typed channels using protocols that can be described by a regular language.  This paper proposes extending session types with a type-level general sequence operation that allows them to express protocols that can be described by context-free languages.  Such an extension is necessary to express, for instance, a protocol for serializing and sending a tree data structure.  The authors present a calculus with context-free session types and give a type safety proof for it.  They show that type equivalence is decidable, which involves a significant amount of metatheoretical machinery, including setting up a bisimulation relation on types to use as the type equivalence relation and using a notion of trace semantics that interpret session types as sets of communication traces to show that it is decidable.

===== Comments for authors and suggestions to make this a strong paper =====

For the most part, I enjoyed reading this paper and found context-free session types to be an interesting and useful idea.  Speaking as a non-expert on session types, though, I found it hard to tell if the paper is a "POPL-sized" contribution.  The related work section seems skimpy for a POPL paper, making it difficult for me to situate this paper's contributions in relation to other work.  The authors themselves admit that the metatheory is quite hairy, and I wonder if the authors can come up with a way to simplify the metatheory while retaining the expressivity of context-free session types.  Therefore I think the paper might benefit from having some more time to mature.  Having said that, I think section 2 is well written and does a great job of illustrating some of the reasons why the metatheory has to be so complicated.

I also have two high-level suggestions for how the paper's presentation could be improved.  First, I think that subsection 3.4 should be its own section and that it should follow section 4.  I can understand that the authors may have wanted the undecidability result in 3.4 to appear earlier in the paper in order to emphasize its importance.  But I think that the paper would be easier to read if section 4 came first.  It is frustrating to have to wait until the last third of a paper to see a language definition.  Second, I think that the proofs (but not the statements of the theorems and lemmas) belong in an appendix instead of in the main text.  As it is, section 3 is quite a long slog for the reader and both of these suggestions would help ameliorate that.

Questions for authors:

  - In the introduction, does "sending a message is naturally polymorphic over the continuation channel" mean that, when sending a message, the type of "whatever happens next" is unconstrained by whatever that message happened to be?

  - At the end of section 2.4 the authors mention in passing an example of a client/server protocol where the server can avoid unncessary communication by inspecting the values already received from the client and deciding whether to ask for more based on those.  Details are left out due to lack of space.  What I'm wondering is whether this is really expressible using even context-free session types; at first glance it seems like it would require some form of dependent typing because the type of the protocol describing the rest of the communication would depend on the values received so far.  (Also, I don't really buy "lack of space" as a reason to leave out the details, because the authors have an entire page they are not using...)

  - The related work section says that the language in this paper differs from previous work in that it uses a synchronous instead of a buffered semantics.  What do "synchronous" and "buffered" mean in this context?  There's no mention of "synchronous" in section 4, which seems like the right place for such an explanation.

Minor presentational suggestions:

  - In section 3.1, the sentence "They furthermore require that their body is contractive in `x`..." I think should read "The formation rule for recursive types requires that...".  The rule for recursive types is the only one that has the contractivity judgment.

  - In section 2.2, I think the the type of `c` should be `TreeChannel; alpha`.

  - In section 2.2, it would be handy to show the typing rule for `send` as well as `select`.

  - Typo: "moniodal" (section 1).

  - Typo: "et at." (section 5).

===========================================================================
                          POPL 2016 Review #111C
---------------------------------------------------------------------------
              Paper #111: Towards Context-Free Session Types
---------------------------------------------------------------------------

                         ===== Paper summary =====

The paper studies an extension of session types - context-free session types. The approach is standard: the authors show an example that cannot be typed in current languages and observe that the problem lies in the semantics. The language of all sequences of communication actions described by a session type is regular whereas the example that they try to type gives rise to a non-regular language. Hence, the authors propose a change in the syntax that enables them to type a larger class of programs, including those that give rise to context-free languages.

===== Comments for authors and suggestions to make this a strong paper =====



There are several challenging things about the problem tackled in this paper. On the one hand, infinite executions are taken into account and hence omega-regular and omega-context free languages are really what its at play, not plain regular or context free languages. This generates problems when it comes to provide the operational semantics and the decidability results. On the other hand the syntax of session types should be minimally changed in order to cover the intended examples but without complicating further the already existing examples. 

I found the paper quite dense and hard to read. I think the authors tried to put too much into one paper and ended up doing so at the cost of the presentation. 

Here are several suggestions I have for improvement: 

- It is not clear to me that the syntax you propose does not allow to define a larger class than context-free session types.
- your use of coinduction is confusing and at points makes me doubt correctness. Example: In section 3.3. you say "This function defines a bisimulation relation". What do you mean by this? Is it an instance of a more general definition? Is it a bisimulation in the sense of Aczel-Mendler? Lemma 3.12: straightforward application of coinduction: why? Page 8, first column: "separate coinductive argument". You really should make this precise. All in all, IMO what you are using is Tarski theorem of greatest fixpoints and not really coinduction in the modern sense of the word. But I might be wrong, so it would have been good to clarify this. 
- The proof of decidability is shaky to say the least: "This automaton is weak by inspection of the transition function."
I would have liked to see this really spelled out, because as the authors know decidability of context free languages is a subtle topic (and I am not even going into omega-context free) and as it stands I feel that there is a lot of handwaving. 

All in all, I feel this paper is not ready yet for publication. It would be great to see the use of coinduction really presented in a clean way. This would open the door to use recent techniques from coinduction and automata theory to develop an efficient type checking algorithm. See e.g. Communication ACM Feb. 2015 (Bonchi & Pous) for a nice overview article on recent work.

===========================================================================
                          POPL 2016 Review #111D
---------------------------------------------------------------------------
              Paper #111: Towards Context-Free Session Types
---------------------------------------------------------------------------

                         ===== Paper summary =====

The paper proposes context-free session types, and extension of
session types. Context-free session types are designed to describe
serialization of tree-structured data. The main result of the paper is that type equivalence is decidable. The result is obtained by
reduction to equivalence of weak omega-DPDAs (deterministic pushdown
automata).

===== Comments for authors and suggestions to make this a strong paper =====

The problem of session types that can describe types for serialization of tree-structured data is interesting and well-motivated. The paper presents this very well in the first two sections. 

Perhaps the main question regarding the paper is whether the result on the decidability of type equivalence is significant enough, or whether the type system would be properly validated only after the type checking algorithm is known (the authors mention that in practice a sound-but-not-complete algorithm might be needed). 

Furthermore: the result that bisimulation between types implies trace
inclusion (3.16) is stated as a conjecture. Is this not needed for
Theorem 3.20 (one of the main results)?

Presentation: The paper would be easier to read if it explained better
where each lemma is used.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             doc/ICFP2016/fig-contractivity.tex                                                                  0000664 0001750 0001750 00000002627 13216017506 017134  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[tp]
  \begin{gather*}
    \frac{}{\GEnv \Contr T_1 \to T_2}% : \Productive}
    \qquad
    \frac{}{\GEnv \Contr T_1 \multimap T_2} % : \Productive}
    \qquad
    \frac{}{\GEnv \Contr T_1 \otimes T_2} % : \Productive}
    \\
    \frac{}{\GEnv \Contr [l_i:T_i]_{i\in I}} % : \Productive}
    \qquad
    \frac{}{\GEnv \Contr B} % : \Productive}
    \qquad
    \frac{}{\GEnv \Contr {!B}} % : \Productive}
    \qquad
    \frac{}{\GEnv \Contr {?B}} % : \Productive}
    \\
    \frac{}{\GEnv \Contr \oplus\{l_i\colon T_i\}_{i\in I}} % : \Productive}
    \qquad
    \frac{}{\GEnv \Contr \&\{l_i\colon T_i\}_{i\in I}} % : \Productive}
    \\
    \frac{}{\GEnv \Contr \skipk} % : \Guarded}
    \qquad
    \frac{\GEnv \Contr T_1 }{ % : \Productive}{
      \GEnv \Contr T_1;T_2} % : \Productive}
    \qquad
    \frac{\GEnv \Contr T_1 % : \Guarded
      \qquad \GEnv \Contr T_2}{ % : \gamma}{
      \GEnv  \Contr (T_1;T_2)} % : \gamma}
    \\
    \frac{\GEnv \Contr T}{\GEnv \Contr \mu x.T } % : \gamma}
    \quad
    \frac{ }{\GEnv, x\colon\kind \Contr x } % : \Productive }
    \quad
    \frac{\GEnv \Contr T}{\GEnv \Contr \forall\alpha::\kind.T } % : \gamma}
    \quad
    \frac{ }{\GEnv, \alpha\colon\kind \Contr \alpha } % : \Productive }
  \end{gather*}
  \caption{Contractivity, $\GEnv \Contr T$} %:\gamma$}
  \label{fig:contractivity}
\end{figure}

%%% Local Variables: 
%%% mode: latex
%%% TeX-master: "main"
%%% End: 
                                                                                                         doc/ICFP2016/example-type-equivalence.txt                                                           0000664 0001750 0001750 00000001675 13216017506 020421  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               S1 = mu x.!B;x
S2 = mu x.!B;x;x

mu x.!B;x == mu x.!B;x;x
unfold left
[mu x.!B;x == mu x.!B;x;x ]
!B; mu x.!B;x == mu x.!B;x;x
unfold right
[mu x.!B;x == mu x.!B;x;x | !B; mu x.!B;x == mu x.!B;x;x]
!B; mu x.!B;x == !B;(mu x.!B;x;x);(mu x.!B;x;x)
step-!B
[mu x.!B;x == mu x.!B;x;x | !B; mu x.!B;x == mu x.!B;x;x]
mu x.!B;x == (mu x.!B;x;x);(mu x.!B;x;x)
unfold left
[mu x.!B;x == mu x.!B;x;x | !B; mu x.!B;x == mu x.!B;x;x | mu x.!B;x == (mu x.!B;x;x);(mu x.!B;x;x)]
!B;(mu x.!B;x) == (mu x.!B;x;x);(mu x.!B;x;x)
unfold right
[mu x.!B;x == mu x.!B;x;x | !B; mu x.!B;x == mu x.!B;x;x
| mu x.!B;x == (mu x.!B;x;x);(mu x.!B;x;x) | !B;(mu x.!B;x) == (mu x.!B;x;x);(mu x.!B;x;x)]
!B;(mu x.!B;x) == !B;(((mu x.!B;x;x);(mu x.!B;x;x));(mu x.!B;x;x))
step-!B
[mu x.!B;x == mu x.!B;x;x | !B; mu x.!B;x == mu x.!B;x;x
| mu x.!B;x == (mu x.!B;x;x);(mu x.!B;x;x) | !B;(mu x.!B;x) == (mu x.!B;x;x);(mu x.!B;x;x)]
(mu x.!B;x) == ((mu x.!B;x;x);(mu x.!B;x;x));(mu x.!B;x;x)
                                                                   doc/ICFP2016/fig-type-equivalence.tex                                                               0000664 0001750 0001750 00000006071 13216017506 017507  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[tp]
  \begin{align*}
    % & F (R) \subseteq R \qquad R \subseteq F (R) \\
    F &:  \Rangeof{S} \times \Rangeof{S} \to \Rangeof{S} \times \Rangeof{S} \\
    F (R) &= \{ (\skipk, \skipk) \} \\
    & \cup \{ (!B, !B) \} \\
    & \cup \{ (?B, ?B) \} \\
    & \cup \{ ((S_1; S_2), (S_1'; S_2')) \mid (S_1, S_1'), (S_2, S_2') \in R \} \\
    & \cup \{ (\oplus\{l_i\colon S_i\}_{i\in I}, \oplus\{l_i\colon S_i'\}_{i\in I}) \mid \{ (S_i, S_i') \mid i\in I\} \subseteq R \} \\
    & \cup \{ (\&\{l_i\colon S_i\}_{i\in I}, \&\{l_i\colon S_i'\}_{i\in I}) \mid \{ (S_i, S_i') \mid i\in I\} \subseteq R \} \\
    & \cup \{ (\mu x.S, S') \mid (S[\mu x.S/x], S') \in R \} \\
    & \cup \{ (S, \mu x.S') \mid (S, S'[\mu x.S'/x]) \in R \} \\
            &\\
    & \cup \{ ((S_1; S_2), S_2') \mid (S_2, S_2') \in R, (S_1, \skipk) \in R \} \\
    & \cup \{ ((S_1; S_2), S_1') \mid (S_1, S_1') \in R, (S_2, \skipk) \in R \} \\
    & \cup \{ (S_1, (S_1'; S_2')) \mid (S_1, S_1') \in R, (S_2', \skipk) \in R \} \\
    & \cup \{ (S_2, (S_1'; S_2')) \mid (S_2, S_2') \in R, (S_1', \skipk) \in R \} \\
    \\
      & \cup \{ ((S;S_3),(S_1';S'))
        \begin{array}[t]{ll}
          \mid
          & ((S_1;S_2), S) \in R,\\
          & (S',(S_2';S_3')) \in R, \\
          & (S_1, S_1') \in R,\\
          & (S_2, S_2') \in R,\\
          & (S_3, S_3') \in R\}
        \end{array}
    \\
      & \cup \{ ((S_1';S'),(S;S_3))
        \begin{array}[t]{ll}
          \mid
          & ((S_1;S_2), S) \in R,\\
          & (S',(S_2';S_3')) \in R, \\
          & (S_1, S_1') \in R,\\
          & (S_2, S_2') \in R,\\
          & (S_3, S_3') \in R\}
        \end{array}
    \\
    % \\
    % & \cup \{ (((S_1;S_2);S_3),S_4) \mid ((S_1;(S_2;S_3)), S_4) \in R\}\\
    % & \cup \{ ((S_1;(S_2;S_3)),S_4) \mid (((S_1;S_2);S_3), S_4) \in R\}\\
    % & \cup \{ (S_1,((S_2;S_3);S_4)) \mid  (S_1,(S_2;(S_3;S_4))) \in R\}\\
    % & \cup \{ (S_1,(S_2;(S_3;S_4))) \mid  (S_1,((S_2;S_3);S_4)) \in
    % R\}\\
    \\
    & \cup
    \begin{array}[t]{ll}
      \{ & ((\oplus\{l_i\colon S_i\}_{i\in I}; S), \oplus\{l_i\colon
      S_i'\}_{i\in I}),
      \\ & ((\&\{l_i\colon S_i\}_{i\in I}; S), \&\{l_i\colon S_i'\}_{i\in I})
      \\ \mid & \qquad \{ ((S_i; S), S_i') \mid i\in I\} \subseteq R \}
    \end{array}
    \\
    & \cup
    \begin{array}[t]{ll}
      \{ & ((\oplus\{l_i\colon S_i\}_{i\in I}), \oplus\{l_i\colon S_i'\}_{i\in I}; S)
      \\ & ((\&\{l_i\colon S_i\}_{i\in I}), \&\{l_i\colon S_i'\}_{i\in I}; S)
      \\\mid& \qquad \{ (S_i, (S_i';S)) \mid i\in I\} \subseteq R \}
    \end{array}
    \\
    \\
    \approx &\subseteq \Rangeof{S} \times \Rangeof{S} \\
    \approx & = \GFP (F)
  \end{align*}
  % \begin{align*}
% %     \cup & \{ ((\skipk;S_1), S_2) \mid (S_1, S_2) \in R\} \\
% %     \cup & \{ ((S_1;\skipk), S_2) \mid (S_1, S_2) \in R\} \\
% %     \cup & \{ (S_1,(\skipk;S_2)) \mid (S_1, S_2) \in R\} \\
% %     \cup & \{ (S_1,(S_2;\skipk)) \mid (S_1, S_2) \in R\}\\
%   \end{align*}
  \caption{Session type equivalence}
  \label{fig:type-equivalence}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                                                                                                                       doc/ICFP2016/fig-types.tex                                                                          0000664 0001750 0001750 00000002205 13216017506 015366  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[t]
  \begin{align*}
    \prekind \grmeq& \kinds \mid \kindt \mid \kindsch  \qquad  \kinds
                     < \kindt < \kindsch &&
\text{Prekinds}
%\text{Prekinds: session, type, or scheme}
    \\
    m \grmeq & \Unrestricted \grmor \Linear \;\quad\qquad \Unrestricted <
               \Linear&&
\text{Multiplicity}
%\text{Multiplicity: unrestricted or linear}
    \\
    \kind \grmeq& \prekind^m && \text{Kinds}
    \\
    T \grmeq& \skipk \mid T;T \grmor \,!B \grmor \,?B  \grmor
    && \text{Types}
    \\
    & \oplus\{l_i\colon T_i\}_{i\in I} \grmor \&\{l_i\colon T_i\}_{i\in I}\\
    & B \grmor T\to T \grmor T\multimap T\\
    & T \otimes T \grmor [l_i\colon T_i]_{i\in I}\\
    & \mu x.T \grmor x \grmor \forall\alpha.T  \grmor \alpha 
    \\
    \Delta \grmeq& \cdot \mid \Delta, x :: \kind \mid \Delta, \alpha
                   :: \kind && \text{Kind environments}
    % \\
    % \Gamma \grmeq& \cdot \mid \Gamma, x\colon T && \text{Type environments}
  \end{align*}
  \caption{Syntax of kinds, types, and kind environments}
  \label{fig:types}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                                           doc/ICFP2016/exploration.cfs                                                                        0000664 0001750 0001750 00000001413 13216017506 015776  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               type XploreTreeChan = oplus{
  Leaf: skip,
  Node: XploreNodeChan
}

type XploreNodeChan = oplus{
  Value: !int;XploreNodeChan,
  Left:  XploreTreeChan;XploreNodeChan,
  Right: XploreTreeChan;XploreNodeChan,
  Exit: skip
}

exploreTree : Tree -> XploreTreeChan;alpha -> alpha
exploreTree Leaf c =
  select Leaf c
exploreTree (Node x l r) c =
  let c1 = select Node c
  in exploreNode x l r c1

exploreNode : int -> Tree -> Tree -> XploreNodeChan;alpha -> alpha
exploreNode x l r c1
  case c1 of {
    Value: lambdac2.let c3 = send c2 x 
               in  exploreNode x l r c3,
    Left:  lambdac2.let c3 = exploreTree l c2
               in  exploreNode x l r c3,
    Right: lambdac2.let c3 = exploreTree r c2
               in  exploreNode x l r c3,
    Exit:  lambdac2.c2
  }
                                                                                                                                                                                                                                                     doc/ICFP2016/trace-equivalence.tex                                                                  0000664 0001750 0001750 00000120200 13216017506 017050  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \subsection{Trace simulation}
\label{sec:trace-simulation}

In this section, we define a trace semantics for session types and prove that type equivalence
coincides with equality of trace sets. Before defining trace languages, we recall some standard
notions from formal language theory. A language $U$ over an alphabet $\Sigma$ is a set of finite
words $U\subseteq \Sigma^*$ with $\varepsilon$ denoting the empty word. Correspondingly, an
$\omega$-language $U\subseteq\Sigma^\omega$ is a 
set of infinite words. We let $\Sigma^\infty = \Sigma^* \cup \Sigma^\omega$ denote the set of finite
and infinite words over $\Sigma$.
We write $|w|$ for the length of a finite word $w\in \Sigma^*$ and define
$|w| = \infty$ if $w \in \Sigma^\omega$. We denote concatenation of words by  the operator
$\cdot :
\Sigma^\infty \times \Sigma^\infty \to \Sigma^\infty$  or just by juxtaposition, if that is unambiguous, with the usual
definition:
\begin{align*}
  u \cdot v &=
              \begin{cases}
                u v & \text{if }|u|<\infty \\
                u & \text{if }|u|=\infty
              \end{cases}
\end{align*}
We define concatenation of ($\omega$-) languages $U\cdot V = \{u\cdot v \mid u\in U,
v \in V \}$ in terms of concatenation of words. 
We abuse notation and sometimes write symbols for single-symbol words and words for single-word
languages. 

\begin{definition}
  Let $\Sigma = \{ {!B}, {?B} \mid B \in \btypes\} \uplus \Tyvars \uplus \{ {\oplus L^i}, {\& L^i} \mid I
  \text{ finite index set}, L =  \{l_j \mid j\in I\}
  \subseteq \Labels, i \in I  \}$ be the alphabet of trace actions where
  a symbol of the form $\oplus L^i$ is a pair of a finite set $L$ of labels that occur together in a choice type and $i$
  is an index identifying an element of $L$. 

%%DONE \vv{Why not use $\oplus L^i$ in place of $\oplus L^i$ (same for $\& L^i$).}
 
A \emph{trace language} is a subset $\TL$ of $\Sigma^\infty$ that
satisfies the following conditions that cover determinacy and
branching.

%% DONE
% \vv{We use $T$ for types. Shall we use $U$ instead? (or $\mathcal U$,
%   since it is a set).}

  \begin{description}
  \item[(T1)] If $uav, ubw \in {\TL}$ with $u\in\Sigma^*$, $v,w \in \Sigma^\infty$, and
    $a \ne b \in \Sigma$, then there exist $L$, $i$, and $j$ such that either  $a=\oplus L^i$ and $b=\oplus L^j$ or $a=\& L^i$ and $b=\& L^j$.
  \item[(T2)]
    If $u\oplus L^i v \in {\TL}$ then there are $v_j$ such that $u\oplus L^jv_j \in {\TL}$, for all $j\ne i \in I (L)$, and
  analogously for $\& L^i$.
  \end{description}
\end{definition}
Condition \textbf{(T1)} says that the only branching points in a trace are the marks of branches
$\& L^i$ and choices $\oplus L^i$ and that they must be used consistently (all marks are either incoming or
outgoing and they must refer to the same label set $L$). Moreover, condition \textbf{(T2)} says that
each branch/choice is exhaustive. 

\begin{definition}\label{def:trace-language}
  Let $S$ be a session type and $\rho$ a mapping from recursion variables to trace languages such
  that $\dom (\rho) \subseteq \Free (S)$.
  The \emph{trace language of a session type} $\TR (S)\rho \subseteq \Sigma^\infty$ is defined by
  induction on $S$.
  \begin{align*}
    \TR (\skipk)\rho &= \{ \varepsilon \} \\
    \TR (\alpha)\rho &= \{ \alpha \} \\
    \TR (!B)\rho &= \{ !B \} \\
    \TR (?B)\rho &= \{ ?B \} \\
    \TR (S_1;S_2)\rho &= \TR (S_1)\rho \cdot \TR (S_2)\rho \\
    \TR (\oplus\{l_i\colon S_i\}_{i\in I})\rho &= {\cup} \{  {\oplus L_i} \cdot \TR (S_i)\rho \mid {i\in I} \} \\
    \TR (\&\{l_i\colon S_i\}_{{i\in I}})\rho &=  {\cup} \{ {\& L_i} \cdot \TR (S_i)\rho \mid {i\in I} \} \\
    \TR (\mu x.S)\rho &= \GFP\, \lambda Y. \TR (S)\rho[x\mapsto Y] \\
    \TR (x) \rho & = \rho (x)
  \end{align*}
\end{definition}

\vv{Merge cases 2--4 as $\TR (A)\rho = \{A\}$.}

\vv{Merge cases 6--7 with $\star$, if you accept my suggestion above
  for $\oplus L^i$.}

\vv{Don't we need braces around $\oplus L^i$ in cases 6--7? ${\cup} \{  \{\oplus L^i\} \cdot \TR (S_i)\rho \mid {i\in I} \}$.}

\vv{A sentence on why the $\GFP$ exists?}

\vv{Why do we need the proviso ``$\dom (\rho) \subseteq \Free (S)$''?
  I assume $\Free (S)$ denotes the set of free \emph{recursion
    variables} in $S$. Then what is the problem of defining
  $\TR (\skipk)[x\mapsto\{!B\}] = \{\varepsilon\}$. In fact, I believe
  that $\TR(\mu x.!B)\emptyset$ would not be defined, nor $\TR(\mu
  x.\oplus\{l\colon x, m\colon!B\})\emptyset$.}

\begin{lemma}
  Let ${\TL} = \TR (S) \rho$ with $S$ and $\rho$ according to Definition~\ref{def:trace-language}. Then
  ${\TL}$ is a trace language.
\end{lemma}
\begin{lemma}
  If $w \in \TR (S) \rho$ and $w$ is branch-free (that is, it does not contain symbols of the form
  $\oplus L^i$ or $\& L^i$), then $\TR (S)\rho = \{w\}$ and either $w$ is finite or $w=  uv^\omega$.
\end{lemma}
\begin{proof}
  By induction on $S$. (Sketch.)

  \textbf{Case }$\skipk$. Finite.

  \textbf{Case }$A$. Finite.

  \textbf{Case }$(S_1;S_2)$. By induction, $\TR (S_1)\rho = \{w_1\}$ and $\TR (S_2)\rho =
  \{w_2\}$. Consider $L = \TR (S_1)\rho \cdot \TR (S_2) \rho$. If $w_1$ is infinite, then $L = \{
  w_1\}$ and the claim holds by induction. If $w_1$ is finite, then $L = \{ w_1w_2 \}$ which is
  finite if $w_2$ is, and $w_1w_2 = w_1 u_2v_2^\omega$ if $w_2=u_2v_2^\omega$.

  \textbf{Case }$\star\{\overline{l_n:S_n}\}$. Contradicts branch-freedom.

  \textbf{Case }$\mu x.S$.
  \begin{align*}
    & w \in \TR (\mu x.S) \rho \\
    & = \GFP\, \lambda Y.\TR (S)\rho[x \mapsto Y] \\
    & = \TR (S) \rho[x \mapsto \GFP\, \lambda Y.\TR (S)\rho[x \mapsto Y]] \\
    & \text{ if $\TR( S)\rho$ independent of $x$,}\\
    & \text{ then the result is immediate by induction} \\
    & = \TR (S) \rho[x \mapsto \emptyset] \\
    & \text{ if $\TR (S) = v\cdot x$, then} \\
    & = \GFP\, \lambda Y.v \cdot Y \\
    & = \{ v^\omega \}
  \end{align*}

\vv{Remove the $w\in$ part from the 1st line?}

\vv{I am a bit puzzled with this proof. I did not expect we could go
  by induction. In a sense $S$ gets smaller, but $\rho$ gets
  larger. Now we are doing induction on $S$, not on $\rho$.  If we can
  proceed by induction then, there is a simpler proof. Let $U$ be the
  trace language $\GFP\, \lambda Y.\TR (S)\rho[x \mapsto Y]$.
%
  \begin{align*}
    & \TR (\mu x.S) \rho && \text{by definition}\\
    =\; & U \\
    =\; & \TR(S)\rho[x\mapsto U] && \text{by I.H.}\\    
    =\; & \{w\}
  \end{align*}
}
\end{proof}

As languages with a mixture of finite and infinite words are cumbersome to deal with, we make the
trace languages into proper $\omega$-languages by completing the finite words with an infinite
sequence of new blank symbols that are added to the alphabet.
\begin{definition}
  Let ${\TL} \subseteq \Sigma^\infty$ be a mixed language and $\BLANK \notin \Sigma$ a new
  symbol. Define   $\widehat{{\TL}} = {\TL} \cdot \BLANK^\omega \subseteq (\Sigma\cup\{\BLANK\})^\omega$. 

  The \emph{$\omega$-trace language of a session type} is defined by $\TRw (S) = \widehat{\TR
    (S)\emptyset} \subseteq (\Sigma\cup\{\BLANK\})^\omega$.
\end{definition}
\begin{lemma}\label{lemma:trw-never-empty}
  Trace languages of well-formed session types are never empty.
\end{lemma}

\begin{lemma}
  \label{lemma:tr-unfold}
  For all substitutions $\sigma$ of recursion variables:
  $\TR (S\sigma)\rho = \TR (\Unfold (S\sigma))\rho$.
\end{lemma}
\vv{Suggestion: ``$\sigma$ from recursion variables to types''}
\begin{proof}
  By induction on $S$. We ignore $\sigma$ when it is not
  needed. \vv{Better if we do not omit; otherwise omissions may look
    like typos.}

  \textbf{Case }$\skipk$. Obvious.

  \textbf{Case }$\alpha$, $!B$, $?B$. Obvious. \vv{use $A$ instead}

  \textbf{Case }$(S_1;S_2)$. Consider the cases for the result of $\Unfold (S_1)$.

  \textbf{Subcase }$\skipk$:
  By induction, $\TR (S_1)\rho = \TR (\skipk)\rho = \{\varepsilon\}$.
  Hence, 
  $\TR (S_1; S_2)\rho = \TR (S_2)\rho =
  \TR (\Unfold (S_1;S_2))\rho$.

  \vv{I guess we also need IH for $\TR(S_2\sigma)\rho$.}

  \textbf{Subcase }$A$:
  By induction, $\TR (S_1)\rho = \TR (A) \rho = \{A\}$.
  Hence,
  $\TR (S_1; S_2)\rho = \{A\} \cdot \TR (S_2)\rho =
  \TR (\Unfold (S_1;S_2))\rho$

  \textbf{Subcase }$(S_3; S_4)$:
  By induction, $$\TR (S_1)\rho = \TR (\Unfold (S_1)) \rho = \TR (S_3;  S_4) \rho.$$
  Hence,
  \begin{align*}
    \TR (S_1;S_2)\rho & = \TR (S_3;S_4)\rho \cdot \TR (S_2) \rho \\
                      &=  \TR (S_3) \rho \cdot  \TR(S_4)\rho \cdot \TR (S_2) \rho\\
                      &= \TR (\Unfold (S_1;S_2))\rho
                        .
  \end{align*}
  \textbf{Subcase }$\star\{l_i\colon S_i\}$: by distibutivity of $\cdot$ over union.

  \textbf{Case }$\star\{l_i\colon S_i\}$: trivial (remains fixed).

  \textbf{Case }$\mu x.S$:
  \begin{align*}
    & \quad\ \TR (\Unfold (\mu x.S\sigma))\rho \\
    & = \{ \text{ by unfold } \} \\
    & \TR (\Unfold(S\sigma[\mu x.S/x]))\rho \\
    & =  \{ \text{ by inductive hypothesis } \} \\
    & \TR (S\sigma[\mu x.S/x])\rho \\
    & =  \{ \text{ by definition TR } \} \\
    & \TR (S\sigma)\rho[x\mapsto \GFP\,\lambda Y. \TR (S)\rho[x\mapsto Y]] \\
    & =  \{ \text{ by fixpoint } \} \\
    & \GFP\,\lambda Y. \TR (S\sigma)\rho[x\mapsto Y] \\
    & =  \{ \text{ by definition TR } \} \\
    & \TR (\mu x.S\sigma)\rho
  \end{align*}
  \vv{There is a $\sigma$ missing in the two lines after unfold as
    well as in the $\GFP$ line; in the first two cases we need
    $S\sigma[\mu x.S\sigma/x]$, the third should be $\TR(S\sigma)$.}

  \vv{Again puzzled. Take $S$ for the type $\mu x.!B;x$. Then
    $\TR(\Unfold(S\emptyset))\emptyset = \TR(\Unfold(S))\emptyset =
    \TR(\Unfold(A;S\emptyset))\emptyset =
    \TR(\Unfold(A;S))\emptyset$.
    Now, $A;S$ is larger than $S$; are you sure one can proceed by
    induction here?}
\end{proof}

To prove equality of trace languages, we extend the coinductive proof techniques developed by Rot
and coworkers \cite{DBLP:conf/lata/RotBR13} to $\omega$-languages. To explain the technique, we consider an LTS $(\Power
(\Sigma^\omega), \Sigma, \LTSderives)$ where the states are $\omega$-languages and the actions the
symbols of the alphabet. The action of a letter $a$ on a language is defined as its derivative:
\begin{align*}
  {\TL} & \LTSderives[a] {\TL}_a := \{ w \in \Sigma^\omega \mid aw \in {\TL} \}
\end{align*}
\begin{definition}\label{def:language-bisimulation}
  A relation $R \subseteq \Power(\Sigma^\omega) \times \Power(\Sigma^\omega)$ is a
 \emph{bisimulation} on $\omega$-languages if
  \begin{description}
  \item[(B1)] $\forall (U, V) \in R$, $\forall a \in \Sigma$, $(U_a,
    V_a) \in R$;

    \vv{$U_a$ may not be defined. The standard definition of bisim
      avoids this case:
      \begin{equation*}
        U \LTSderives[a]  U' \text{ implies } V \LTSderives[a] V'
        \wedge U' R V'
      \end{equation*}
      (we also need the symmetric case.)
    }

  \item[(B2)] $(\emptyset,V)  \in R$ implies $V = \emptyset$;
  \item[(B3)] $(U, \emptyset) \in R$ implies $U = \emptyset$.
  \end{description}
  Bisimilarity $\bisim$ is the union of all bisimulations.
\end{definition}
Bisimilarity in this LTS corresponds to equality of trace languages.
\begin{proposition}
  For all $U, V \subseteq \Sigma^\omega$,  $U=V$ implies $U\bisim V$. 
\end{proposition}
\begin{proof}
  Every bisimulation is an equivalence relation.
\end{proof}
\begin{proposition}\label{proposition:bisim=equality}
  For all trace languages $U, V \subseteq \Sigma^\omega$,  $U\bisim V$ implies $U=V$. 
\end{proposition}
\begin{proof}
  Suppose
  that $U \bisim V$. If $U=\emptyset$, then $V=\emptyset$ by requirement~(B2). If $U\ne\emptyset$,
  then we show that $w \in U$ implies $w\in V$. To do so, we consider prefixes $w[n] = w_0\dots
  w_{n-1}$, for $n\in\Nat$, where $w_i \in\Sigma$ is the $i$th symbol of $w$, and show that for all $n\in\Nat$,
  $w_nw_{n+1}\ldots \in U_{w[n]} \ne \emptyset$ and $U_{w[n]} \bisim V_{w[n]}$. As this result implies $V_{w[n]}
  \ne\emptyset$, for all $n\in\Nat$, it must be that $w \in V$.

  \textbf{Case }$0$ is immediate.

  \textbf{Case }$n>0$: Assume by the inductive hypothesis that $U_{w[n]} \bisim V_{w[n]}$ and let
  $a=w_n$ so that $w[n+1] = w[n]a$. By requirement~(B1), $U_{w[n+1]} \bisim V_{w[n+1]}$,
  $w_{n+1}w_{n+2}\ldots \in U_{w[n+1]} \ne \emptyset$, and hence $V_{w[n+1]} \ne \emptyset$.
\end{proof}

Given this result, it is sufficient to establish a bisimulation between languages $U$ and $V$ to
prove that $U=V$. We exploit this fact to prove the next proposition.

\begin{proposition}
  If $S_1 \TypeEquiv S_2$, then $\TRw (S_1) = \TRw (S_2)$.
\end{proposition}
\begin{proof}
  It is sufficient to establish a bisimulation $Q \supseteq (\TRw (S_1), \TRw (S_2))$. To this end
  define 
  \begin{align*}
    Q &= \{ (\TRw (S_1), \TRw (S_2)) \mid S_1 \TypeEquiv S_2 \} \cup \{(\emptyset, \emptyset)\}
  \end{align*}
  The requirements~(B2) and~(B3) from Definition~\ref{def:language-bisimulation} are fulfilled because
  $\TRw (S)$ is never empty by Lemma~\ref{lemma:trw-never-empty}. It remains to show that $\forall
  a\in\Sigma$, $(U,V) \in Q$ implies $(U_a, V_a) \in Q$ by cases on the definition of $F$. We only
  consider guarded session types because unfolding does not affect the trace language (Lemma
  \ref{lemma:tr-unfold}). 
  \begin{enumerate}
  \item $(\TRw (\skipk), \TRw (\skipk)) = (\{\$^\omega\}, \{\$^\omega\}) \in Q$. On symbol $\$$
    remain fixed. On any other symbol, both side transition to $\emptyset$ and
    $(\emptyset,\emptyset) \in Q$.
  \item $(\TRw (A), \TRw (A)) = (\{A\cdot\$^\omega\}, \{A\cdot\$^\omega\}) \in Q$. On symbol $A$,
    both sides transition to state $\{\$^\omega\}$ and $(\{\$^\omega\}, \{\$^\omega\}) \in Q$. On
    any other symbol, both sides transition to $\emptyset$.
  \item $(\TRw (A;S_1), \TRw (A;S_2)) = (A\cdot\TRw (S_1), A\cdot\TRw (S_2)) \in Q$. On a symbol
    $\ne A$ both sides transition to $\emptyset$. On symbol $A$, we need to check that $(\TRw
    (S_1),\TRw (S_2)) \in Q$, which holds because $(A;S_1) \TypeEquiv (A;S_2)$ implies $S_1
    \TypeEquiv S_2$. 
  \item $(\TRw (\star\{\overline{l_n:S^1_n}\}), \TRw (\star\{\overline{l_n:S^2_n}\})) \in Q$ implies
    that $(\TRw (S^1_i), \TRw (S^2_i)) \in Q$, for all $i$, because $S^1_i \TypeEquiv S^2_i$.
  \end{enumerate}
  The claim follows by Proposition~\ref{proposition:bisim=equality}.
\end{proof}

\begin{lemma}
  If  $\TRw (S_1) \subseteq \TRw (S_2)$, then  $S_1 \TypeEquiv S_2$.
\end{lemma}
\begin{proof}
  Let $R = \{ (S_1, S_2) \mid \TRw (S_1) \subseteq \TRw (S_2) \}$ and show that $R
  \subseteq  F(R)$. 
  By Lemma~\ref{lemma:tr-unfold}, it is sufficient to consider the unfolded guarded types
  for $(S_1, S_2)$.

  \textbf{Case }$(\skipk, \skipk) \in F (\emptyset)$.

  \textbf{Case }$(A, A) \in F (\emptyset)$.

  \textbf{Case }$((A;S_1'), (A; S_2'))$. From $\TRw (A; S_1') = \{A\} \cdot \TRw (S_1') \subseteq
  \{A\} \cdot \TRw (S_2') = \TRw (A; S_2')$ it follows that $\TRw (S_1') \subseteq \TRw (S_2')$ so that
  $(S_1', S_2') \in R$. Hence, $(S_1, S_2) \in F (R)$.

  \sloppypar
    \textbf{Case
    }$(\star\{l_i\colon S_{1,i}\}, \star\{l_i\colon
    S_{2,i}\})$.
    Here we require the closedness condition on trace languages to
    conclude from
    $\TRw (\star\{l_i\colon S_{1,i}\}) = {\cup} \{ L_i \cdot \TRw
    (S_{1,i}) \} \subseteq {\cup} \{ L_i \cdot \TRw (S_{2,i}) \} =
    \TRw (\star\{l_i\colon S_{2,i}\}) $
    that $\TRw (S_{1,i}) \subseteq \TRw (S_{2,i})$, for all $i$, which
    in turn means $(S_{1,i}, S_{2,i}) \in R$, for all $i$. Hence
    $(S_1, S_2) \in F(R)$.
  %\end{sloppypar}
\end{proof}
\clearpage{}
Next, we set out to prove that $\TRw (S)$ is an $\omega$-context-free language. To do so, we a
construct a PDA from $S$ that recognizes the same language. We may assume that each recursion variable
in $S$ is uniquely bound, that is, for each $x\in S$, there is exactly one subterm of $S$ of the
form $\mu x.S'$.
We exploit this restriction to define a ``lazy'' version of unfolding with respect to some $S$ as follows.
\begin{enumerate}
\item $\Unfold_S(\mu x.T) = \Unfold_S(T)$
\item $\Unfold_S (S_1;S_2) = \left\{%
  \begin{array}{ll}
    \Unfold_S(S_2) & \Unfold_S(S_1) = \skipk
    \\
    (A; S_2) & \Unfold_S(S_1) = A
    \\
    (x; S_2) & \Unfold_S(S_1) = x
    \\
    (S_3; (S_4;S_2)) & \Unfold_S(S_1) = (S_3;S_4)
    \\
    \star\{l_i\colon S'_i;S_2\}  & \Unfold_S (S_1) = \star\{l_i\colon S'_i\}
  \end{array}
  \right.
$
%\item $\Unfold_S (x) = \Unfold_S(T)$, where $\mu x.T$ is the unique subterm of $S$ determined by $x$
\item $\Unfold_S(T) = T$, otherwise
\end{enumerate}
Define the (finite) set of all lazy unfoldings of subterms of a given session type $S$:
\begin{align*}
  \Unfold^* (S)& = \{ \Unfold_S (S') \mid S' \text{ subterm of }S \}.  
\end{align*}
All types in this set are guarded by construction or their ``first action'' is defined by a
recursion variable $x$.

We may further assume that the only occurrences of $\skipk$ in $S$ appear in the otherwise empty branches
of branch and choice types. Each well-formed session type may be transformed into this $\skipk$-reduced form.


Now we are ready to define a PDA $\PDA(S)$ for $S$.
The set of stack symbols of $\PDA (S)$ is $\Gamma = \Unfold^* (S)$.
The set of states is $Q = \{ q_0, q \}$.
The transition function is defined by
\begin{enumerate}
\item $q_0 \to [\Unfold_S (S)] q$
\item $[\skipk] q \to q$
\item $[A] q A \to q$
\item $[A;S'] q A \to [\Unfold_S (S')] q$
\item $[\star\{l_i:S_i\}] q L_j \to [\Unfold_S (S_j)] q$
\item\label{item:4} Let  $\mu x.S'$ be the subterm of $S$  determined by $x$:
  \begin{enumerate}
  \item\label{item:5} $[x] q \to [\Unfold_S(S')] q$ 
  \item\label{item:6} $[x; S''] q \to [\Unfold_S (S'')][\Unfold_S(S')] q$ 
  \end{enumerate}
\end{enumerate}

This automaton is deterministic, but it has some transitions that do not consume input (cf.\ Item~\ref{item:4}). These
$\varepsilon$-transition may be eliminated by identifying the symbols $[x]$ and $[\Unfold_S(S')]$,
which eliminates the transitions according to Item~\ref{item:5}, and by pulling back the transition
of Item~\ref{item:6} to the transition that pushes a symbol of the form $[x; S'']$. Contractivity
ensures that this transformation of the PDA terminates. Hence, there is a real-time DPDA that
accepts the same language as $\PDA (S)$.

The PDA $\PDA (S)$ accepts a language by the trivial acceptance criterion (i.e., existence of a run is
sufficient to accept a word).

\begin{lemma}
  $\TR (S)\emptyset = L (\PDA(S))$.
\end{lemma}
\begin{proof}
  First we need to become more concrete. Let words $u,v,w$ range over $\Sigma^\infty$ unless
  otherwise indicated. 

  $\forall w$: $w \in \TR (S)\emptyset$ iff there is a run for $[\Unfold_S (S)] q w$.

  Induction on $S'$ as a subterm of $S$.

  \textbf{Case }$\skipk$:\\
  $w = \TR (\skipk)\emptyset= \varepsilon$ and a run $[\skipk] q \varepsilon
  \vdash q$ obviously exists.

  \textbf{Case }$A$: \\
  $w = \TR (A)\emptyset= A$ and there is a run $[A] q A \vdash q$.

  \textbf{Case }$(S_1;S_2)$:\\
  $w = \TR (S_1;S_2) \emptyset= \TR (S_1) \emptyset \cdot \TR (S_2)\emptyset$

  Case analysis on $S_1' = \Unfold_S (S_1)$.

  \textbf{Subcase }$S_1' = \skipk$: In this case, $\TR (S_1) \emptyset = \{\varepsilon\}$, $\Unfold_S
  (S_1;S_2) = \Unfold_S (S_2)$, and the result holds by induction on $S_2$.

  \textbf{Subcase }$S_1' = A$: In this case,  $\TR (S_1) \emptyset = \{ A \}$, $\Unfold_S (S_1;S_2)
  = (A; S_2)$, $w = Aw'$, $w' \in \TR (S_2)\emptyset$, and there is a transition $[A;S_2] q A \vdash
  [\Unfold_S (S_2)] q$. By induction on $S_2$, there is a run for $[\Unfold_S (S_2)] q w'$.

  \textbf{Subcase }$S_1' = \star\{l_i:S_i\}$: In this case, $\TR (S_1) \emptyset = \bigcup_i \{ L_i
  \} \cdot \TR (S_i)\emptyset$, $\Unfold_S (S_1;S_2) = \star\{l_i:(S_i; S_2)\}$, $w = L_j w'$, $w'
  \in \TR(S_j; S_2)\emptyset$, and there is a transition $[\star\{l_i:(S_i; S_2)\}] q L_j \vdash
  [\Unfold_S (S_j; S_2)]q $. \textbf{Need a terminating order so that we can argue by induction
    here:} By induction, $w'  \in \TR(S_j; S_2)\emptyset$ iff exists a run for $[\Unfold_S (S_j;
  S_2)]q w'$.

  \textbf{Subcase }$S_1' = (S_3;S_4)$: In this case,  $\TR (S_1) \emptyset = \TR (S_3)\emptyset
  \cdot \TR (S_4) \emptyset$, $\Unfold_S (S_1;S_2) = (S_3;(S_4; S_2))$, $w = uv$, and $\TR
  (S_3)\emptyset = u $, $\TR (S_4;S_2)\emptyset = v$.

  We need to construct a run for $[(S_3;(S_4; S_2))] q uv$. \textbf{To this end, we need to be able to argue
  by induction }
  that $[(S_3;(S_4; S_2))] q u v \vdash^* [\Unfold_S (S_4;S_2)] v $ and that the latter configuration has
  a run.

  \textbf{Subcase }$S_1' = x$: In this case, $\TR (S_1)\emptyset = \dots$ \textbf{Here, it's not
    clear how to replace $\emptyset$ by a meaningful environment.}
\end{proof}

\clearpage

\begin{theorem}\label{theorem:tr-is-w-CFL}
  $\TRw (S)$ is an $\omega$-context-free language.
\end{theorem}
\begin{proof}
  We may assume that the only occurrences of $\skipk$ in $S$ appear in the otherwise empty branches
  of branch and choice types. Each well-formed session type may be transformed in this $\skipk$-reduced form.

  For the translation from $S$ to the productions of a context-free grammar, we rely on a monad that
  is a combination of an output monad to collect the set of productions and a name generator monad
  for fresh nonterminal symbols. The monadic functions \Out{} and \Fresh{} output a production and
  return a new nonterminal symbol drawn from a set $\NT$. We use a Haskell-inspired \Do{} notation
  for composing monadic functions.
  \begin{figure}[tp]
    \begin{align*}
      \toLHS (\skipk)\SEnv & = \Return\ \varepsilon \\
      \toLHS (A)\SEnv & = \Return\ A \\
      \toLHS (S_1; S_2)\SEnv & = \Do
                              \begin{array}[t]{l}
                                \gamma_1 \gets \toLHS (S_1) \\
                                \gamma_2 \gets \toLHS (S_2) \\
                                \Return (\gamma_1\cdot\gamma_2)
                              \end{array}
      \\
      \toLHS (\star\{\overline{l_n:S_n}\})\SEnv &= \Do
                                                 \begin{array}[t]{l}
                                                   N_\star \gets \Fresh \\
                                                   \dots \\
                                                   \gamma_i \gets \toLHS (S_i)\SEnv \\
                                                   \Out (N_\star \to \star L^i \cdot \gamma_i) \\
                                                   \dots \\
                                                   \Return\ N_\star
                                                 \end{array}
      \\
      \toLHS (\mu x.S)\SEnv &= \Do
                             \begin{array}[t]{l}
                               N_\mu^x \gets \Fresh \\
                               \gamma \gets \toLHS (S)\SEnv[x \mapsto N_\mu^x] \\
                               \Out (N_\mu^x \to \gamma) \\
                               \Return\ N_\mu^x
                             \end{array}
      \\
      \toLHS (x) \SEnv &= \Return\ (\SEnv (x))
      \\
      \toCFG (S) &= \Do
                   \begin{array}[t]{l}
                     N_0 \gets \Fresh \\
                     \gamma \gets \toLHS (S)\emptyset \\
                     \Out (N_0 \to \gamma) \\
                     \Return\ N_0
                   \end{array}
    \end{align*}
    \caption{Translation of a session type to a CFG}
    \label{fig:session-to-grammar}
  \end{figure}

  Figure~\ref{fig:session-to-grammar} defines the translation with two functions $\toCFG$ and $\toLHS$. The entry point
  is $\toCFG$ which creates the start symbol $N_0$ of the grammar and invokes $\toLHS$ to create the
  right hand side of the production. The function $\toLHS$ takes a session type $S$ and an environment
  $\SEnv$ mapping recursion variables to nonterminals. It returns a string of terminals and
  nonterminal and creates new productions as needed. New productions are started for each branch or
  choice type to model the alternative and for each $\mu$-type to model the recursion.

  If the session type $S$ is  $\skipk$-reduced, then the generated grammar has no
  $\varepsilon$-productions. Moreover, the grammar is deterministic because for each nonterminal there is
  either just one rule (for the start symbol $N_0$ and for the symbols $N_\mu$ used to model
  recursion) or each rule starts with a distinct terminal symbol (for the branch and choice symbols $N_\star$). 

  It remains to show that the thus generated grammar defines $\TR (S)\emptyset$. To this end, we let
  $N_0 \gets \toCFG (S)$ and call the generated set of productions (extracted from the output monad)
  $P$. That is, formally we have to show that $\TR (S) \emptyset = L^\infty (P)( N_0)$, which may be
  defined as the projection on $N_0$ of the greatest fixpoint of the productions $P$
  \cite{Niwinski1984}. This fixpoint is a nonterminal-indexed vector of $\infty$-languages which we
  write equivalently as a mapping $\rho$ from nonterminals to $\infty$-languages:
  \begin{align*}
    L^\infty (P) & = \GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv) \\
    \RHS (P, N, \LEnv) &= \bigcup_{N \to \gamma \in P} \hat\LEnv({\gamma}) & \gamma & \in (\Sigma \cup
                                                                                 \NT)^* \\
    \hat\LEnv (\varepsilon) &= \{\varepsilon\} \\
    \hat\LEnv (a \gamma)  &= \{ a \} \cdot \hat\LEnv (\gamma)  & a &\in \Sigma\\
    \hat\LEnv (N \gamma) &= \LEnv (N) \cdot \hat\LEnv (\gamma) & N & \in \NT
  \end{align*}

  We show by induction on the execution of $\gamma \gets \toLHS(S)\SEnv$ that $\TR (S) \rho =
  \hat\LEnv (\gamma) $ where $\LEnv = \GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv)$ with $P$
  being the final output of the grammar generation and, for all $x\in\Free (S)$, $\rho (x) = \LEnv (N_\mu^x)$.

  \textbf{Case }$\skipk$. Immediate.

  \textbf{Case }$A$. Immediate.

  \textbf{Case }$(S_1;S_2)$. By induction, $\TR (S_i) \rho = \hat\LEnv (\gamma_i)$ where $\gamma_i
  \gets \toLHS (S_i)\SEnv$ so that $\toLHS (S_1;S_2)\SEnv$
  returns $\gamma_1\cdot\gamma_2$. Hence $\TR (S_1;S_2)\rho = \TR (S_1)\rho \cdot \TR (S_2)\rho =
  \hat\LEnv (\gamma_1\cdot\gamma_2)$ as desired.

  \textbf{Case }$\star \{ \overline{l_n:S_n} \}$. By induction, $\TR (S_i) \rho = \hat\LEnv
  (\gamma_i)$ where $\gamma_i \gets \toLHS (S_i)\SEnv$ so that $\toLHS (\star \{ \overline{l_n:S_n}
  \})\SEnv$ returns some $N_\star$ with productions $N_\star \to \star L^i \cdot \gamma_i \in P$.
  Hence, by unrolling the fixpoint, 
  \begin{align*}
    \hat\LEnv (N_\star) & = \LEnv (N_\star) \\
                        & = \bigcup_{N_\star \to \gamma \in P} \hat\LEnv  (\gamma) \\
                        &= \bigcup_i \hat\LEnv (\star L^i \cdot \gamma_i)  \\
                        &=\bigcup_i \{\star L^i\} \cdot\hat\LEnv (\gamma_i)  \\
                        &=\bigcup_i \{\star L^i\} \cdot\TR (S_i)\rho  \\
                        &=\TR( \star\{ \overline{ l_n: S_n}\} )\rho
  \end{align*}
\clearpage
  \textbf{Case }$\mu x.S$.
  Let $N_\mu^x$ be the fresh nonterminal generated for this recursion and
  $\gamma \gets \toLHS (S)\SEnv[x \mapsto N_\mu^x]$ its right hand side.
  By induction,
  $\TR (S) \rho[x \mapsto \hat\LEnv (N_\mu^x)] = \hat\LEnv(\gamma)$.
  \begin{align*}
    \hat\LEnv (N_\mu^x) &= \LEnv (N_\mu^x) \\
                        &= \hat\LEnv (\gamma) \\
                        &= \TR (S) \rho[x \mapsto \hat\LEnv (N_\mu^x)] \\
                        &=? \\
                        &= \TR (S) \rho[x \mapsto \GFP\ \lambda Y. \TR (S)\rho[x \mapsto Y]] \\
                        &= \GFP\ \lambda Y. \TR (S) \rho[x \mapsto Y] \\
                        &= \TR (\mu x.S)\rho \\
                        &\\
    & \text{Apply David Park's lower fixpoint induction principle:} \\
    \hat\LEnv (\gamma) & \subseteq \GFP\ \lambda Y. \TR (S) \rho[x \mapsto Y] \\
    \text{iff } & \exists X. X \subseteq \TR (S) \rho[x \mapsto X] \wedge \hat\LEnv (\gamma)
                   \subseteq X \\
    \text{choose } & X = \hat\LEnv (\gamma) \\
    \text{then } & \hat\LEnv (\gamma) \\
                        &\stackrel{IH}= \TR (S)\rho[x \mapsto \LEnv (N_\mu^x)] \\
                        &= \TR (S)\rho[x \mapsto (\GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv)) (N_\mu^x)] \\
                        &= \TR (S)\rho[x \mapsto \RHS (P, N_\mu^x, \LEnv)] \\
                        &= \TR (S)\rho[x \mapsto \hat\LEnv (\gamma)] \\
  \end{align*}
  For the reverse direction, we want to prove that
  \begin{align}\label{eq:3}
    \GFP\ \lambda Y. \TR (S) \rho[x \mapsto Y] & \subseteq (\GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv)) (N_\mu^x)
  \end{align}
  Now we can apply Bekic-Leszczylowski to the right hand side to pull out the fixpoint on component
  $N_\mu$.
  \begin{align*}
    & (\GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv)) (N_\mu^x) \\
    &= \GFP\, \lambda Y. \RHS (P, N_\mu, (\GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv[N_\mu
      \mapsto Y])) ) \\
    &= \GFP\, \lambda Y.
      \begin{array}[t]{l}
        \hat\delta (\gamma) \\
        \text{where}\ \delta = (\GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv[N_\mu
      \mapsto Y]))
      \end{array}
  \end{align*}
  Now we need $X$ such that
  \begin{align*}
    X &\subseteq \begin{array}[t]{l}
        \hat\delta (\gamma) \\
        \text{where}\ \delta = (\GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv[N_\mu
      \mapsto X]))
      \end{array}
    \\
    & \GFP\ \lambda Y. \TR (S) \rho[x \mapsto Y] \subseteq X
  \end{align*}
  But all this requires the statement that we want to prove \dots

  To proceed, we blow up the left hand side of Equation~\eqref{eq:3} to a vector by taking the
  components for $N \ne N_\mu$ from the right hand side.
  \begin{align*}
    & \GFP\, \lambda \LEnv. [N_\mu \mapsto \TR (S) \rho[x \mapsto \LEnv(N_\mu)], N \mapsto \RHS (P, N, \LEnv)]
      \\
    & \subseteq
      (\GFP\, \lambda \LEnv. \lambda N. \RHS (P, N, \LEnv))
  \end{align*}
  To apply Park's lower fixpoint induction principle again, we need to find an environment $\xi$ such that 
  \begin{align}\label{eq:4}
    && \xi & \subseteq \lambda N. \RHS (P, N, \xi)  \\
    \notag\Leftrightarrow &
           & \forall N. \xi (N) & \subseteq \RHS (P, N, \xi)
  \end{align}
  and
  \begin{align}\label{eq:5}
    \GFP\, \lambda \LEnv.
    \left[
    \begin{array}{l}
      N_\mu \mapsto \TR (S) \rho[x \mapsto \LEnv(N_\mu)] \\
      N \mapsto \RHS (P, N,    \LEnv)
    \end{array}
    \right]
 &\subseteq \xi
  \end{align}
  
  Let's try with $\xi=\delta$, the fixpoint of the rhs.
  Clearly, Equation~\eqref{eq:4} holds because $\delta$ is a fixpoint.

  Unfolding the lhs of Equation~\eqref{eq:5} and expanding pointwise yields
  \begin{align}
    \TR (S) \rho[x \mapsto (\GFP\dots)(N_\mu)] & \subseteq \LEnv (N_\mu)  = \hat\delta (\gamma)\\
    \RHS (P, N,  (\GFP\dots)) & \subseteq \LEnv (N) & N \ne N_\mu
  \end{align}

  \begin{align*}
    \toCFG (\skipk, N) & = \{ N \to \varepsilon \} \\
    \toCFG (\alpha, N) & = \{ N \to \alpha \} \\
    \toCFG (!B, N) & = \{ N \to !B \} \\
    \toCFG (?B, N) & = \{ N \to ?B\} \\
    \toCFG ((S_1; S_2), N) & = \{ N \to N_1N_2 \} \cup \toCFG (S_1, N_1) \cup \toCFG (S_2, N_2) \\
    \toCFG (\oplus\{l_i\colon S_i\}, N) & = \{ N \to {\oplus L_i} N_i \mid i\in I \} \cup \toCFG (S_i, N_i) \\
    \toCFG (\&\{l_i\colon S_i\}, N) & = \{ N \to {\& L_i} N_i \mid i\in I \} \cup \toCFG (S_i, N_i) \\
    \toCFG (\mu x.S, N) &= \{ N \to N_x \} \cup \toCFG (S, N_x) \\
    \toCFG (x, N) &= \{ N \to N_x \} \\
  \end{align*}
  Next, we show that this grammar defines $\TR (S)\emptyset$.
  In particular, $\TR (S)\emptyset = L^\infty (\toCFG (S, N_0), N_0)$
  using the greatest fixpoint characterization of the
  $\infty$-language defined by a context-free grammar $G$ using
  nonterminal $N$ as the start symbol:
  \begin{align*}
    L^\omega (G, N) & = (\GFP\, \lambda Y. \lambda N. \RHS (N, Y)) (N) \\
    \RHS (N, Y) &= \bigcup
                          \begin{cases}
                            \{ \varepsilon \} & N \to \varepsilon \in G \\
                            \{ x \} & N \to x \in G\\
                            Y (N_1) \cdot Y (N_2) & N \to N_1N_2 \in G \\
                            \{ x \} \cdot Y (N') & N \to x N' \in G \\
                            Y (N') & N \to N' \in G
                          \end{cases}
  \end{align*}
  In this definition, $Y$ maps a nonterminal symbol to its associated
  $\omega$-language. The function $\RHS (N, Y)$ takes the union of the
  interpretations of all right-hand sides for $N$ (as
  $\omega$-languages) using $Y$ to interpret the nonterminal symbols.

  The proof of the language equality proceeds by induction on $S$.

  \textbf{Case }$\skipk$, $\alpha$, $!B$, $?B$. Immediate.

  \textbf{Case }$(S_1;S_2)$:
  \begin{align*}
    & \TR (S_1;S_2)\rho \\
    &= \TR (S_1)\rho \cdot \TR (S_2)\rho \\
    &= L^\omega (\toCFG (S_1, N_1), N_1) \cdot L^\omega( \toCFG (S_2, N_2), N_2) \\
    & = L^\omega (\{ N_0 \to N_1N_2\} \cup \toCFG (S_1, N_1) \cup \toCFG (S_2, N_2), N_0) \\
    & = L^\omega (\toCFG ((S_1; S_2), N_0), N_0)
  \end{align*}

  \textbf{Case }$\oplus\{l_i\colon S_i\}$:
  \begin{align*}
    & \TR (\oplus\{l_i\colon S_i\})\rho \\
    & =  {\cup} \{  {\oplus L_i} \cdot \TR (S_i)\rho \mid {i\in I} \} \\
    & =  {\cup} \{  {\oplus L_i} \cdot L^\omega (\toCFG(S_i, N_i), N_i) \} \\
    & =  L^\omega (\{ N_0 \to {\oplus L_i} N_i \mid i\in I \} \cup \toCFG (S_i, N_i), N_0) \\
    & = L^\omega (\toCFG (\oplus\{l_i\colon S_i\}, N_0))
  \end{align*}

  \textbf{Case }$\&\{l_i\colon S_i\}$: Analogously.

  \textbf{Case }$\mu x.S$:
  \begin{align*}
    & \TR (\mu x.S)\rho \\
    &= \GFP\, \lambda Y. \TR (S)\rho[x\mapsto Y] \\
    & \{ \text{ by a separate coinductive argument } \} \\
    &= L^\omega (\toCFG (S, N_x) , N_x)\\
    &= L^\omega (\{ N_0 \to N_x \} \cup \toCFG (S, N_x) , N_0)\\
    &= L^\omega (\toCFG (\mu x.S, N_0), N_0)
  \end{align*}
  The coinductive argument relies on the coinductive definition of $L^\omega$.
  To establish the needed inclusion in both directions, consistency must be proved with respect to
  the other monotone function.

  Finally, we embed $L \subseteq \Sigma^\infty$ in a proper $\omega$-language by choosing a new
  symbol $\BLANK \notin \Sigma$  and considering $\hat{L} = L \cdot \BLANK^\omega \subseteq
  (\Sigma\cup\{\BLANK\})^\omega$. To adjust the grammar generated from $S$ with start symbol $N_0$,
  we add two new symbols $N_0'$ and $N_\bot$ and two productions: $N_0' \to N_0N_\bot$ and $N_\bot
  \to \BLANK N_\bot$.
\end{proof}

We aim to show that equivalence of trace languages of session types is decidable.
Because $\TRw (S)$ is a deterministic context-free $\omega$-language, it can be recognized by
a deterministic $\omega$-pushdown automaton. While equivalence of standard deterministic
pushdown automata is known to be decidable \cite{DBLP:conf/icalp/Senizergues97}, decidability is only
known for equivalence of \emph{weak} $\omega$-deterministic pushdown automata \cite{DBLP:conf/mfcs/LodingR12}, but
not for the full class.
% Unfortunately, weak $\omega$-pushdown automata are not sufficient to recognize $\TR(S)\emptyset
% \cdot \BLANK^\omega$ as soon as $\TR (S)\emptyset$ contains infinite words.

To make this point precise, we recap the relevant definitions.
\begin{definition}
  $\MACHINE= (Q, \Sigma, \Gamma, \delta, q_0, \bot)$ is
  a \emph{deterministic pushdown automaton} (DPDA) if it consists of
  \begin{itemize}
  \item a finite set $Q$ of states and initial state $q_0\in Q$,
  \item a finite input alphabet $\Sigma$; we write $\Sigma_\varepsilon = \Sigma \cup\{\varepsilon\}$,
  \item a finite stack alphabet $\Gamma$ with initial stack symbol $\bot\notin\Gamma$; we write
    $\Gamma_\bot = \Gamma\cup\{\bot\}$,
  \item a transition function
    $\delta \in (Q \times \Gamma \times \Sigma_\varepsilon \to Q  \times \Gamma^*)
    \cup (Q \times \{\bot\} \times \Sigma_\varepsilon \to Q \times  \Gamma^*\bot)$, such that for each $q \in Q$ and
    $Z\in \Gamma_\bot$ either
    $\delta (q, Z, x)$ is defined for all $x \in\Sigma$ and $\delta (q, Z, \varepsilon)$ is
    undefined and vice versa.
  \end{itemize}
  A configuration of $\MACHINE$ is an element of $Q \times \Gamma^*\bot$ with initial
  configuration $(q_0,\bot)$. A \emph{run} of $\MACHINE$ on $w \in \Sigma^\omega$ is an infinite sequence
  of configurations $(q_i,\gamma_i)_{i\in\Nat}$ such that $\gamma_0 = \bot$, $w =
  \sigma_0\sigma_1 \dots$ where $\sigma_i \in \Sigma_\varepsilon$, 
  $\gamma_i = Z_i\gamma_i'$, $\gamma_{i+1} = g_i\cdot\gamma_i'$, and $\delta (q_i, Z_i, \sigma_i) =
  (q_{i+1}, g_i)$.

  A run is \emph{accepting}, if the parity of the highest priority of an infinitely occuring state
  is even, where priority is given by a map $\Omega : Q \to \Nat$. (Parity acceptance criterion.)

  A machine is \emph{weak}, if no transition decreases the priority: for each transition from $q$ to
  $q'$ it must be that $\Omega (q) \le \Omega (q')$.
\end{definition}

\begin{proposition}\label{prop:trace-is-weak-wpda}
  The language $\TRw (S)$ can be recognized by a weak $\omega$-DPDA.
\end{proposition}
\begin{proof}
  To construct an $\omega$-DPDA for this language, we consider the context-free grammar $G = \toCFG
  (S)$ with set of nonterminals $\mathcal{N}$ and start symbol $N_0 \in \mathcal{N}$ and transform
  it to a DPDA $(Q, \Sigma', \Gamma, \delta, q_0, \bot)$ as shown in Figure~\ref{fig:construction-of-dpda}. 
  \begin{figure}[t]
    \begin{align*}
      Q & = \{q_0, q_2, q_4 \} && \text{with priorities given by the indexes} \\
      \Sigma' & = \Sigma \cup \{ \BLANK \} &
      \Gamma & = \Sigma \cup \mathcal{N} \\
      \delta & \ni (q_0, \bot, \varepsilon, q_2, N_0\bot)  && \\
      \delta & \ni (q_2, N, \varepsilon, q_2, \varepsilon)  & N & \to \varepsilon\\
      \delta & \ni (q_2, N, A, q_2, \varepsilon)  & N & \to A \qquad A \in \{\alpha, !B, ?B \}\\
      \delta & \ni (q_2, N, \varepsilon, q_2, N_1N_2)  & N & \to N_1N_2\\
      \delta & \ni (q_2, N, \oplus L_i, q_2, N_i)  & N& \to \oplus L_iN_i\\
      \delta & \ni (q_2, N, \& L_i, q_2, N_i)  & N& \to \& L_iN_i\\
      \delta & \ni (q_2, N, \varepsilon, q_2, N_x)  & N& \to N_x\\
      \delta & \ni (q_2, \bot, \BLANK, q_4, \bot) \\
      \delta & \ni (q_4, \bot, \BLANK, q_4, \bot) 
    \end{align*}
    \caption{Construction of DPDA}
    \label{fig:construction-of-dpda}
  \end{figure}
In the definition  of $\delta$ the mapping on the left is part of delta if the production on the
right is a production of the grammar.

This automaton is weak by inspection of the transition function. It is standard to prove that it recognizes the same
$\omega$-language as the grammar.
\end{proof}

Here is an example for an automaton resulting from the construction from the preceding proposition. 
\begin{align*}
  S & = (l_1 : {?B}; S; !B) \oplus (l_2 : \skipk)
  \\
  \TR (S) \emptyset & = \{ (\oplus L_1.?B)^n.\oplus L_2.{!B}^n \mid n \in \Nat\} \cup (\oplus L_1; ?B)^\omega
\end{align*}
The grammar associated to $S$ has start symbol $S'$:
\begin{align*}
  S' & \to S\ S'' & S'' & \to \BLANK\ S'' \\
  S & \to {\oplus L_1} {?B} S {!B} \mid {\oplus L_2} 
\end{align*}
The pushdown automaton derived from the grammar looks as follows (with some simplifications to improve readability).
\begin{align*}
  Q &= \{ q_0, q_2, q_4 \}  \qquad \text{with priorities $0$, $2$, and $4$, respectively}\\
  \delta & \in Q \times \Gamma_\bot \times \Sigma_\varepsilon \to Q \times (\Gamma^* \cup \Gamma^*\bot) \\
  \delta & = \{
           \begin{array}[t]{l}
             (q_0, \bot, \varepsilon, q_2, S\bot) \\
             (q_2, S, {\oplus L_1}, q_2, {?B} S {!B}) \\
             (q_2, S, {\oplus L_2}, q_2, \varepsilon) \\
             (q_2, {?B}, {?B}, q_2, \varepsilon) \\
             (q_2, {!B}, {!B}, q_2, \varepsilon) \\
             (q_2, \bot, \BLANK, q_4, \bot) \\
             (q_4, \bot, \BLANK, q_4, \bot) \\
             \} \\
           \end{array}
\end{align*}
An infinite run of the automaton either remains in state $q_2$ accepting a word of the form $(\oplus L_1;
?B)^\omega$ or it steps up to $q_4$ accepting a word with paired up $?B$s and $!B$s
and remains there (by weakness). 
\begin{theorem}
  Equivalence of session types is decidable.
\end{theorem}
\begin{proof}
  $S_1 \TypeEquiv S_2$ iff $\TRw (S_1) = \TRw (S_2)$ iff $L^\omega (\toCFG (S_1,
  N_1), N_1) = L^\omega (\toCFG (S_2, N_2), N_2)$. By Proposition~\ref{prop:trace-is-weak-wpda},
  there are weak $\omega$-DPDAs that recognize the trace languages for $S_1$ and for
  $S_2$. Equivalence of weak $\omega$-DPDAs is known to be decidable~\cite{DBLP:conf/mfcs/LodingR12}.
\end{proof}


% Nevertheless, we abandon completeness and use a sound approximation to bisimulation equivalence
% $\TypeEquiv$ because its checking algorithm is simpler. 

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                                                doc/ICFP2016/processes.tex                                                                          0000664 0001750 0001750 00000076623 13216017506 015504  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \section{Processes, Statics, and Dynamics}
\label{sec:processes}

This section introduces our programming language, its static and
dynamic semantics. It shows that typing is preserved by reduction and
concludes by showing that our type system is a conservative extension
of that of a conventional functional session type language.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Expressions and Processes}

\input{fig-processes}
 
Fix a base set of \emph{term variables}, disjoint from those
introduced before. Let $a,b$ range over this set.
%
The syntax of values, expressions, and processes is described in
Figure~\ref{fig:processes}.

\emph{Expressions}, denoted by metavariable $e$, incorporate a
\emph{standard functional core} composed of term variables~$a$,
abstraction introduction $\lambda a.e$ and elimination $ee$, pair
introduction $(e,e)$ and elimination $\letin{a,b} e e$, datatype
introduction $\inject le$ and elimination
$\match e {l_i\rightarrow e_i}_{i\in I}$, as well as a fixed point
construction $\fixk\,e$.
%
Further expressions support the usual \emph{session operators}, in the
form of channel creation $\newk$, message sending $\sendk$ and
receiving $\recvk$, internal choice (or label selection) $\select le$,
and external choice (or branching)
$\case e{\{l_i\rightarrow e_i\}_{i\in I}}$.
%
Concurrency arises from a $\forkk$ operator, spawning
a new process.
%
% In expressions $\match e {l_i\rightarrow e_i}_{i\in I}$ and
% $\casek\,e\,\ofk\,\{l_i\rightarrow e_i\}_{i\in I}$ $I$ is an index set.

\emph{Processes}, denoted by metavariable $p$, are expressions~$e$,
the parallel composition of two processes~$p\PAR q$, and the scope
restriction~$(\nu a,b)p$ of a channel described by its two end
points,~$a$ and~$b$.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Operational Semantics}
\label{sec:semantics}

% Bindings, free vars, substitution

The \emph{binding} occurrences for term variables~$a$ and~$b$ are
expressions $\lambda a.e$, $\fixk\,a.e$, and $\letin{a,b}{e_1}{e_2}$.
%, and $\case e{\{l_i\rightarrow e_i\}_{i\in I}}$.
The sets of free and bound
term variables are defined accordingly, and so is the
\emph{capture avoiding substitution} of a variable~$a$ by a value~$v$ in a term~$e$,
denoted by $e\subs va$.

The operational semantics makes use of a \emph{structural congruence}
relation on processes, $\equiv$, defined as the smallest congruence
relation that includes the commutative monoidal rules---binary
operator $\_\PAR\_$ and any value for neutral---and scope extrusion:
%
\begin{equation*}
  (\nu a,b)p \PAR q \equiv (\nu a,b)(p\PAR q) \quad\text{if $a,b$ not free
    in $q$}
\end{equation*}

% op semantics

The operational semantics is call-by-value: expressions are reduced to
values before being ``applied''. The syntax of values is in
Figure~\ref{fig:processes}, and includes the values $\sendk$,
$\recvk$, unit, lambda abstraction, pair of values, and injection of a
value in a sum type. The semantics combines a standard reduction
relation for the functional part with an also standard message passing
semantics of the $\pi$-calculus.
%
The rules are in Figure~\ref{fig:reduction}.

\input{fig-reduction}

The first four axioms are standard in functional call-by-value languages,
and comprise $\beta$-reduction, (linear) pair elimination, data type
elimination, and fixed-point unrolling.
%
The first rule in the figure allows reduction to happen underneath
(functional) evaluation contexts $E$ as defined by the grammar
below.
%
\begin{align*}
  E \grmeq& [] \grmor (E,e) \grmor (v,E) \grmor Ee \grmor vE \grmor \letin{a,b}Ee
  % \letin aEe \grmor
  \\
  \grmor& \casek\,E\,\ofk\,\{l_i\rightarrow e_i\} \grmor \selectk\,l\,E
  \\
  \grmor& \match{E}{l_i\rightarrow e_i} \grmor \ink\,l\,E
\end{align*}

The $\forkk$ operator creates new threads: the expression $\fork{e}$
evaluates to $()$, the unit value, while creating a new thread to run
expression~$e$ concurrently.

The next three axioms in the figure deal with session operations. The
$\newk$ operator creates a new channel. Channels are denoted by their
two end points,~$a$ and $b$ in this case. We require that context~$E$
does not bind variables $a,b$, so that these are bound by the
outermost channel binding, $(\nu a,b)$.

The $\sendk$-$\recvk$ rule captures message passing: the sending
process writes value~$v$ in channel end point~$a$ whereas the
receiving process reads it from channel end point~$b$. That the pair
$a$-$b$ forms the two end points of a channel is captured by the
outermost $(\nu a,b)$ binding. The result of sending a value on channel
end~$a$ is~$a$ itself; that of receiving on~$b$ is the pair
$(v,b)$. In this way both threads are able to use their channel ends
for further interaction. This ``rebinding'' of channel ends provide
for a standard treatment of~$a$ and~$b$ as linear values. It is also
the type system that makes sure that, in a given process, there is
exactly one thread holding a copy of a given channel end, thus
allowing a simplified reduction rule where one finds exactly two
threads underneath channel binder $(\nu a,b)$.

The rule for $\selectk$-$\casek$ is similar in spirit. One thread
selects an $l$-labelled option on a channel end, whereas another offers
a choice on the other channel end. After successful interaction, the
selecting thread is left with its channel end, $a$, whereas the
choice-offering thread is left with an application of the
branch that was selected, $e_j$, to its channel end, $b$.

The remaining three rules are standard in the $\pi$-calculus. The
first two allow reduction underneath parallel composition and scope
restriction; the last incorporates structural congruence in the
reduction relation.

% Example of reduction

As an example consider an expression that creates a new channel, forks
a thread that writes an integer on the channel and reads back its
successor. The original thread, in turn, waits for an integer value
and writes back its successor. We depict the reduction below, where we
make use of the conventional semicolon operator $e_1;e_2$ defined as
$(\lambda a.e_2)e_1$, where $a$ is a variable not occurring free
in~$e_2$. We also write $\letin{a}{e_1}{e_2}$ for $(\lambda a.e_2)e_1$.
%
\begin{gather*}
  \letk\, a,b = \newk\,\ink\,
    \forkk\,(\letin{c}{\send 5a}{\recv c});\qquad \qquad \qquad
    \\\qquad \qquad \qquad \qquad \qquad
    \letin{n,d}{\recv b}{\send{(n+1)}{d}}
  \rightarrow
  \\
  (\nu e,f)\letin{a,b}{(e,f)}{\dots} \rightarrow
  \\
  (\nu e,f) \forkk\,(\letin{c}{\send 5e}{\dots});
    \letin{n,d}{\recv f}{\dots} \rightarrow \rightarrow
  \\
  (\nu e,f) (\letin{c}{\send 5e}{\dots} \PAR \letin{n,d}{\recv f}{\dots})
  \rightarrow
  \\
  (\nu e,f) (\letin{c}{e}{\dots} \PAR \letin{n,d}{(5,f)}{\dots})
  \rightarrow \rightarrow 
  \\
  (\nu e,f) (\recv e \PAR \send{(5+1)}{f})
  \rightarrow
  \\
  (\nu e,f)((e,6) \PAR f)
\end{gather*}

% RUNTIME ERRORS

We complete this section by discussing the notion of \emph{run-time
  errors}. The \emph{subject} of an expression~$e$, denoted by
$\subj(e)$, is~$a$ in the following cases and undefined in all other
cases.
%
\begin{equation*}
  \send ea \qquad
  \recv a \qquad
  \select ea \qquad
  \case ae
\end{equation*}

We say that two expressions~$e_1$ and $e_2$ \emph{agree} on channel
$ab$, denoted $\agree^{ab}( e_1,e_2)$, in the following four cases.
%
\begin{itemize}
\item $\agree^{ab}(\send ea, \recv b)$;
\item $\agree^{ab}(\recv a,\send be)$;
\item $\agree^{ab}(\select{l_j}{a}, \case b{l_i\rightarrow e_i}_{i\in
    I})$ and $j\in I$;
\item $\agree^{ab}(\case a{l_i\rightarrow e_i}_{i\in
    I}, \select{l_j}{b})$ and $j\in I$.
\end{itemize}

A closed process is an \emph{error} if it is structurally congruent to
some process that contains a subexpression or subprocess of one of the
following forms.
%
\begin{enumerate}
\item $\letin{a,b}{v}{e}$ and $v$ is not a pair;
\item $\match{v}{l_i\rightarrow e_i}_{i\in I}$ and
  $v\ne(\ink\,l_j\,v')$ for some $v'$ and some $j\in I$;
\item $E_1[e_1] \PAR E_2[e_2]$ and $\subj(e_1) = \subj(e_2) = a$, where
  neither $E_1$ nor $E_2$ bind~$a$;
\item $(\new a,b)(E_1[e_1] \PAR E_2[e_2] \PAR p)$ and $\subj(e_1)=a$
  and $\subj(e_2)=b$ and $\neg\agree^{ab}(e_1,e_2)$, where $E_1$ does not
  bind~$a$ and $E_2$ does not bind~$b$.
% \item $  (\new a,b)(E_1[\selectk\,l_j\,a] \PAR
%   E_2[\casek\,b\,\ofk\,\{l_i\rightarrow e_i\}_{i\in I}] \PAR p) $
%   where $E_1$ does not bind $a$ and $E_2$ does not bind $b$ and
%   $j\notin I$ (or the same with $a$ and $b$ exchanged).
\end{enumerate}

The first two cases are typical of functional languages with pairs and
datatypes.
%
The third case guarantees that no two threads hold references to the
same channel end (the fact that the process is closed and that the
contexts do not bind variable~$a$ ensure that~$a$ is a channel end).
%
The fourth case says that channel ends agree at all times: if one
thread is ready for sending, then the other is ready for receiving,
and similarly for selection and branching.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Type Assignment System}

% DUALITY

\emph{Duality} is a central notion in session types. It allows to
``switch'' the point of view from one end of a channel (say, the
client side) to the other (the server side).
%
The duality function on session types, $\dual S$, is defined as
follows.
%
\begin{gather*}
  \dual\alpha = \alpha 
  \qquad
  \dual\skipk = \skipk
  \qquad
  \dual{!B} = \;?B
  \qquad
  \dual{?B} = \;!B
  \\
  \dual{\&\{l_i\colon S_i\}} = \oplus\{l_i\colon \dual{S_i}\}
  \qquad
  \dual{\oplus\{l_i\colon S_i\}} = \&\{l_i\colon \dual{S_i}\}
  \\
  \dual{S_1;S_2} = \dual{S_1};\dual{S_2}
  \qquad
  \dual{\mu x.S} = \mu x.\dual S
  \qquad
  \dual x = x
\end{gather*}
%
This simple definition is justified by the fact that the types we
consider are first order, thus avoiding a complication known to arise
in the presence of
recursion~\cite{bernardi.hennessy:using-contracts-model-session-types}.

To check whether $S_1$ is dual to $S_2$ we compute $S_3 = \dual{S_1}$
and check $S_2$ and $S_3$ for equivalence.
%
Duality is clearly an involution ($\dual{\dual S} = S$), hence we can
alternatively compute $\dual{S_2}$ and check that $S_1$ is equivalent
to $\dual{S_2}$.
%
For example, to check that $!B;\mu x.(\skipk;!B;x)$ is dual to
$\mu y.(?B;y)$, we compute $\dual{\mu y.(?B;y)}$ to obtain
$\mu y.(!B;y)$, and check that this type is equivalent to
$!B;\mu x.(\skipk;!B;x)$.

We can easily show that duality preserves kinding.

\begin{lemma}
  \label{lem:duality-preserves-kinding}
  If $\Delta \vdash S :: \kind$, then $\Delta \vdash \dual S :: \kind$.
\end{lemma}
%
\begin{proof}
  By rule induction on the premise.
\end{proof}

% TYPING CONTEXT

\input{fig-wellformed-contexts}

Typing contexts are generated by the following grammar.
%
\begin{equation*}
  \Gamma \grmeq \cdot \grmor \Gamma,a\colon T
\end{equation*}
%
As before we consider contexts up to reordering of their entries.
%
% The UN PREDs
%
The $\un_\Delta$ predicate, on types~$T$ defined on $\Delta$, is an
abbreviation of $\Delta \vdash T :: \prekind^\Unrestricted$. The $\un_\Delta$
predicate is also true of contexts of the form
$x_1\colon T_1,\dots, x_n\colon T_n$ if it is true of all
types~$T_1,\dots,T_n$. We often omit the $\Delta$ in $\un_\Delta$ when
it is clear from the context.

% CONTEXT FORMATION
We expect all types in typing contexts to be well formed, a notion
captured by judgement $\Delta \vdash \Gamma$ whose rules can be found
in Figure~\ref{fig:contexts}.

% CONTEXT SPLITTING
Linear variables must be split between the different subterms to
ensure that each variable is used once. Figure~\ref{fig:contexts}
defines a relation $\Delta \vdash \Gamma = \Gamma_1 \circ \Gamma_2$,
which describes how to split a context $\Gamma$ into two contexts
$\Gamma_1$ and $\Gamma_2$ that will be used in different subterms in
rule premisses~\cite{walker:substructural-type-systems}.

%
% However, unlike conventional contexts ($\Delta$ for example), we
% allow duplicated variables in contexts, but subject to two
% restrictions: if both $a\colon T$ and $a\colon U$ are in $\Gamma$
% then: i) $T=U$ and ii) $\Delta \vdash T :: \kindt^\Unrestricted$.


% TYPING

\input{fig-typing}

Figure~\ref{fig:typing} contains the typing rules for expressions and for
processes. Judgments for expressions and processes take the usual forms of
$\Delta;\Gamma \vdash e: T$ and 
$\Delta;\Gamma \vdash p$. We describe the rules briefly.

% description of the rules

The first group of rules deals with the functional part of
the language. The rule for the unit value requires a context
free from term variables. If needed, unrestricted term variables are
introduced by an explicit weakening rule. The typing rule for variables
reads the type of the variable from the context. We require that
the term context contains no other entry, and that the type is
well-formed against~$\Delta$, ensuring that types introduced in a
derivation are well-formed. The rule for the fixed point is standard.

The type system comprises rules for the introduction and the
elimination of unrestricted ($\rightarrow$) and linear ($\multimap$)
functions.
%
The elimination rules are standard. In the introduction of linear
functions the term context is split in two parts, one to type the
function~$e_1$, the other to type the argument~$e_2$.
%
% The next rule in the same line allows unrestricted functions to be
% converted into linear functions, so that the rule for function
% elimination may apply.

The next four rules are all standard and provide for the
introduction and the elimination of pairs ($T_1\otimes T_2$) and
variants ($[l_i\colon T_i]$).
%
The rules for the introduction and elimination of type abstraction are
also conventional; the extra premises on kindings are meant to ensure
that types introduced in derivations are well-formed.

We now come to the channel communication rules. The rule for channel
creation introduces a pair of dual session types, one for each end
point. The $\sendk$ operator is a function that expects a value to be
sent~$B$, then a channel on which to send the value~$!B;T$, and
returns the rest of the channel~$T$. The $\recvk$ operator expects a
channel from which a value can be read $?B;T$ and returns a pair
composed of the value and the rest of the channel, $B\otimes T$. The
premises ensure that~$T$ is a session type.
%
The rule for label selection requires that expression~$e$ denotes a
channel offering an internal choice, $\oplus\{l_i\colon
T_i\}$.
Expression $\select {l_j} e$ evaluates to the rest of the channel,
hence its type is~$T_j$.
%
A $\casek$ expression expects a channel offering an external choice,
$\&\{l_i\colon T_i\}$. The expression in each branch must be function
expecting the rest of the channel~$T_i$. All such functions must
produce a value of a common type~$T$, which becomes the type of the
$\casek$ expression.

The rule for $\forkk$ requires the expression to be of an unrestricted
type, for the value the expression evaluates to will never be
consumed.

The last three rules are structural. The first two ---weakening and
copy (or contraction)---manipulate the term context. In both cases
we require the type to be unrestricted. The last rule incorporates
type equivalence in the typing relation.

The rules for processes should be easy to understand. An expression,
when seen as a process must be of an unrestricted type. This implies
that linear resources, channels in particular, are fully consumed. The
rule for parallel composition splits the context in two, using one
part for each process. Finally, the rule for channel creation
introduces two entries in the context, of types dual to each other,
one for each end of the channel.

We complete this section with a result that relates the type system to
the kinding system.

\begin{lemma}[Agreement]
  If $\Delta;a_1\colon T_1,\dots,a_n\colon T_n \vdash e : T_0$, then,
  for all $0\le i\le n$, there are kinds $\kind_i$ such that
  $\Delta \vdash T_i:: \kind_i$.
\end{lemma}
%
\begin{proof}
  By rule induction on the premise using the various kinding
  preservation lemmas (\ref{lem:subs-preserves-kinding},
  \ref{lem:equiv-preserves-kinding}, and
  \ref{lem:duality-preserves-kinding}).
\end{proof}

% The usual derived rules.
% %
% \begin{gather*}
%   \frac{
%     \Gamma_1 \vdash e_1:T_1
%     \quad
%     \Gamma_2,x:T_1 \vdash e_2:T_2
%   }{
%     \Gamma_1,\Gamma_2 \vdash \letin x {e_1}{e_2} : T_2
%   }
% \\
%   \frac{
%     \Gamma_1 \vdash e_1:T_1
%     \quad
%     \Gamma_2 \vdash e_2:T_2
%     \quad
%     \un(T_1)
%   }{
%     \Gamma_1,\Gamma_2  \vdash e_1;e_2 : T_2
%   }
% \end{gather*}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Soundness and Type Safety}

The proofs for the two results of this section follow a conventional
approach: we first establish lemmas for strengthening, weakening,
% (weakening is built into the type system) -- Not anymore
substitution, sub-derivation manipulation, and inversion of the typing
relation. Soundness (Theorem~\ref{thm:soundness}) follows by rule
induction on the reduction step, and type safety
(Theorem~\ref{thm:safety}) follow by an analysis of the typing
derivation.

\begin{lemma}[Strengthening]
  \label{lem:strengthening}
  If $\Delta;\Gamma,a\colon T \vdash p$ and~$a$ not free in~$p$, then
  $\Delta;\Gamma \vdash p$ and~$\un_\Delta(T)$.
\end{lemma}
%
\begin{proof}
  By rule induction on the first premise.
\end{proof}

\begin{lemma}[Weakening]
  \label{lem:weakening}
  If $\Delta;\Gamma \vdash p$ and $\Delta \vdash T::\prekind^\Unrestricted$, then
  $\Delta;\Gamma, a\colon T \vdash p$.
\end{lemma}
%
\begin{proof}
  By rule induction on the first premise.
\end{proof}

\begin{lemma}[Congruence]
  \label{lem:congruence}
  If $\Delta;\Gamma \vdash p$ and $p\equiv q$, then
  $\Delta;\Gamma \vdash q$.
\end{lemma}
%
\begin{proof}
  By rule induction on the first premise, using strengthening
  (Lemma~\ref{lem:strengthening}).
\end{proof}

\begin{lemma}[Substitution]
  \label{lem:subs}
  If $\Delta;\Gamma_1, a\colon T_2 \vdash e_1 : T_1$ and
  $\Delta;\Gamma_2 \vdash e_2 : T_2$, then
  $\Delta;\Gamma_1,\Gamma_2 \vdash e_1 \subs{e_2}{a} : T_1$.
\end{lemma}
%
\begin{proof}
  By rule induction on the first premise.
\end{proof}

The following two lemmas are adapted from~\cite{DBLP:journals/jfp/GayV10}.

\begin{lemma}[Sub-derivation introduction]
  \label{lem:derivation-intro}
  If $\mathcal D$ is a derivation of $\Delta;\Gamma \vdash E[e] : T$,
  then there exist $\Gamma_1$, $\Gamma_2$ and $U$ such that
  $\Gamma = \Gamma_1,\Gamma_2$ and $\mathcal D$ has a sub-derivation
  $\mathcal D'$ concluding $\Delta;\Gamma_2 \vdash e : U$ and the
  position of $\mathcal D'$ in $\mathcal D$ corresponds to the
  position of the hole in $E$.
\end{lemma}

\begin{lemma}[Sub-derivation  elimination]
  \label{lem:derivation-elim}
  If
  \begin{itemize}
  \item $\mathcal D$ is a derivation of
    $\Delta;\Gamma_1,\Gamma_2 \vdash E[e] : T$,
  \item $\mathcal D'$ is a sub-derivation of $\mathcal D$
    concluding $\Delta;\Gamma_2 \vdash e : U$,
  \item the position of $\mathcal D'$ in $\mathcal D$ corresponds
    to the position of the hole in $E$,
  \item $\Delta;\Gamma_3 \vdash e_2 : U$,
  \item $\Gamma_1,\Gamma_3$ is defined,
  \end{itemize}
then
  $\Delta;\Gamma_1,\Gamma_3 \vdash E[e_2] : T$.
\end{lemma}

% No more combined rule
% The structural rules (weakening, copy, and $\TypeEquiv$) commute.  We
% can easily show that these three rules can be replaced by a single
% combined rule as follows.
% %
% \begin{equation*}
%   \frac{
%     \Delta;\Gamma_1, \Gamma_2, \Gamma_2 \vdash e : T_1
%     \quad
%     \un_\Delta(\Gamma_2,\Gamma_3)
%     \quad
%     \Delta \vdash T_1 \TypeEquiv T_2
%   }{
%     \Delta;\Gamma_1, \Gamma_2, \Gamma_3 \vdash e : T_2
%   }
% \end{equation*}
% %
% Notice that if we replace, in the weakening rule, the proviso
% $a\notin\Gamma$ by
% $a:U\in\Gamma \Rightarrow \Delta \vdash U\TypeEquiv T$, then copy and
% weakening do not commute anymore.  This combined rule forms the basis
% for the inversion lemma below.

\begin{lemma}[Inversion of the expression typing relation]\
  \label{lem:inversion}

% Cases ordered as required for soundness, which in turn are ordered
% by the reduction rules.

  \begin{itemize}
  \item % app
    If $\Delta;\Gamma \vdash e_1e_2 : T$, then
    $\Gamma = \Gamma_1 \circ \Gamma_2$ with 
    $\Delta;\Gamma_2 \vdash e_2: T_1$ and
    $\Delta \vdash T_2 \TypeEquiv T$ and either
    \begin{itemize}
    \item $\Delta;\Gamma_1 \vdash e_1: T_1 \multimap T_2$; or 
    \item $\Delta;\Gamma_1 \vdash e_1: T_1 \rightarrow T_2$.
    \end{itemize}
  \item % lambda
    If $\Delta;\Gamma \vdash \lambda a.e : T$, then
    $\Delta;\Gamma, a:T_1 \vdash e: T_2$ and
    $\Delta \vdash T_1, T_2 ::\kindt^\Linear$
    either
    \begin{itemize}
    \item $\un_\Delta (\Gamma_1)$ and $\Delta \vdash T_1 \to T_2
      \TypeEquiv T$; or
    \item  $\Delta \vdash T_1 \multimap T_2      \TypeEquiv T$.
    \end{itemize}
  \item % let/2
    If $\Delta;\Gamma \vdash \letin{a,b}{e_1}{e_2} : T$, then
    $\Gamma = \Gamma_1 \circ \Gamma_2$ and
    $\Delta;\Gamma_1 \vdash e_1: T_1 \otimes T_2$ and
    $\Delta \vdash T \TypeEquiv U$ and
    $\Delta;\Gamma_2 ,a\colon T_1,b\colon T_2 \vdash e_2: U$.
  \item % (e1,e2)
    If $\Delta;\Gamma \vdash (e_1, e_2) : T$, then
    $\Gamma = \Gamma_1\circ \Gamma_2$ with 
    $\Delta;\Gamma_1 \vdash e_1: T_1$ and
    $\Delta;\Gamma_2 \vdash e_2: T_2$ and
    $\Delta \vdash T_1, T_2 ::\kindt^\Linear$ and
    $\Delta \vdash T_1\otimes T_2 \TypeEquiv T$.
  \item % match
    If $\Delta;\Gamma \vdash \match{e}{l_i\rightarrow e_i}_{i\in
      I} : T$, then
    $\Gamma =  \Gamma_1 \circ \Gamma_2$ with
    $\Delta; \Gamma_1 \vdash e : [l_i : T_i]$ and
    $\Delta; \Gamma_2 \vdash e_i : U$ and
    $\Delta \vdash U \TypeEquiv T$.
  \item % in
    If $\Delta;\Gamma \vdash \ink\,l_j\,e : T$, then
    $\Delta;\Gamma \vdash e: T_j$ and
    $\Delta \vdash [l_i:T_i]_{i\in I} \TypeEquiv T$ and
    $j\in I$ and
    $\Delta\vdash T_i::\kindt^m$, for all $i\in I$.
  \item % fix
    If $\Delta;\Gamma \vdash \fix ae : T$, then
    $\un_\Delta(\Gamma)$ and
    $\vec\alpha\notin\Delta$ and
    $\Delta, \vec\alpha::\vec\kind;\Gamma,
    a:\forall\vec\alpha::\vec\kind. U \vdash e: U$ and
    $\Delta \vdash \forall\vec\alpha::\vec\kind. U \TypeEquiv T$.
  \item % let
    If $\Delta;\Gamma \vdash \letin{a}{e_1}{e_2} : T$, then
    $\Gamma = \Gamma_1 \circ \Gamma_2$ and
    $\vec\alpha\notin\Delta$ and
    $\Delta, \vec\alpha::\vec\kind; \Gamma_1 \vdash e_1 : T_1$ and
    $\Delta;\Gamma_2,
    a:\forall\vec\alpha::\vec\kind. T_1 \vdash e_2: T_2$ and
    $\Delta \vdash \forall\vec\alpha::\vec\kind. T_2 \TypeEquiv T$.
  \item % fork
    If $\Delta;\Gamma \vdash \fork\,e:T$, then
    $\un_\Delta(U,\Gamma)$ and
    $\Delta;\Gamma \vdash e : U$ and
    $T = \unitk$.
  \item % new
    If $\Delta;\Gamma \vdash \newk : T$,
    then $\un_\Delta(\Gamma)$ and
    $\Delta \vdash T \TypeEquiv S\otimes\dual S$ and
    $\Delta \vdash T::\kinds^m$.
  \item % send
    If $\Delta;\Gamma \vdash \sendk\, e\, a : T$, then
    $\Delta \vdash T \TypeEquiv S$ and
    $\Gamma = \Gamma_1, a\colon T_2$ and
    $\Delta \vdash T_2 \TypeEquiv \;!B;S$ and
    $\Delta;\Gamma_1 \vdash e \colon B$.
  \item % receive
    If $\Delta;\Gamma \vdash \recvk\, a : T$, then
    $\Delta \vdash T \TypeEquiv B \otimes S$ and 
    $\Gamma = \Gamma_1, a\colon T_2$ and
    $\un_\Delta(\Gamma_1)$ and
    $\Delta \vdash T_2 \TypeEquiv \;?B;S$.
  \item % select
    If $\Delta;\Gamma \vdash \select{l_j} a : T$, then
    $\Delta \vdash T \TypeEquiv S_j$ and
    $\Gamma = \Gamma_1, a\colon T_2$ and
    $\un_\Delta(\Gamma_1)$ and $\Delta \vdash T_2 \TypeEquiv
    \oplus \{ l_i\colon S_i \}_{i\in I}$ and $j\in I$.
  \item % case
    If $\Delta;\Gamma \vdash \casek\,b\,\ofk\,\{l_i\rightarrow
    e_i\}_{i\in I} : T$, then
    $\Gamma =  \Gamma_1, b\colon T_2$ and  
    $\Delta \vdash T_2 \TypeEquiv  \& \{ l_i\colon S_i \}$ and
    (for all $i\in I$)
    $\Delta; \Gamma_1 \vdash e_i : S_i \tcLolli T'$ and
    $\Delta \vdash T' \TypeEquiv T$.
  % \item %par
  %   If $\Delta;\Gamma \vdash p \PAR q$, then
  %   $\Gamma = \Gamma_1, \Gamma_2, \Gamma_3, \Gamma_4$ and
  %   $\un_\Delta (\Gamma_3,\Gamma_4)$ and
  %   $\Delta; \Gamma_1, \Gamma_3 \vdash p$ and
  %   $\Delta; \Gamma_2, \Gamma_3 \vdash q$.
  % \item %new
  %   If $\Delta;\Gamma \vdash (\new a,b) p$, then there exists $T$
  %   such that $\Delta; \Gamma, a:T, b:\dual{T} \vdash p$.
  % \item If $\Delta;\Gamma \vdash p$ , then there exists $q$ such that
  %   $p \equiv q$ and $\Delta;\Gamma \vdash q$.
  \end{itemize}
\end{lemma}
%
\begin{proof}
  For each case we consider the derivation ending with the
  corresponding structural rule followed by the combined rule, and
  collect all undischarged assumptions.
\end{proof}

Inversion of the typing relation for processes is obtained by simply
reading the rules for processes bottom-up, since all rules are
syntax-directed.

\begin{theorem}[Soundness]
  \label{thm:soundness}~\\
  If $\Delta;\Gamma \vdash p$ and $p \rightarrow q$, then
  $\Delta;\Gamma \vdash q$.
\end{theorem}
%
\begin{proof}
  By rule induction on the second premise, using the congruence and
  the substitution lemmas, sub-derivation introduction and
  elimination, and inversion
  (Lemmas~\ref{lem:congruence}--\ref{lem:inversion}).

  % Cases by the order the reduction rules appear in the definition
  % (figure X).

  \textbf{Case} the derivation ends with $\beta$: inversion (for
  expressions as processes, application, and abstraction),
  substitution lemma, rule for expressions as processes.

  \textbf{Case} the derivation ends with $\letk$: inversion (for
  expressions as processes, $\letk$, and pairs), substitution lemma
  (twice), weakening and copy rules, rule for expressions as
  processes.

  \textbf{Case} the derivation ends with $\matchk$: inversion (for
  expressions as processes, $\matchk$, and $\ink$), rules for
  application and expressions as processes.

  \textbf{Case} the derivation ends with $\fixk$: inversion (for
  expressions as processes and $\fixk$), substitution lemma, 
  contraction and rule for expressions as processes.

  \textbf{Case} the derivation ends with context: inversion for
  expressions as processes, sub-derivation intro, induction,
  sub-derivation elim, and rule for expressions as processes.

  \textbf{Case} the derivation ends with $\forkk$: inversion for
  expressions as processes, sub-derivation intro, inversion for
  $\forkk$, rule $()$, sub-derivation elimination, combined rule,
  rules for expressions as processes and parallel composition.

  \textbf{Case} the derivation ends with $\newk$: inversion for
  expressions as processes, sub-derivation intro, inversion for
  $\newk$, var axiom, $\otimes$ intro, sub-derivation elimination,
  typing rules for expressions as processes and $\nu$.

  \textbf{Case} the derivation ends with the reduction rule for
  communication: inversion ($\nu$, parallel composition, and
  expressions as processes twice), sub-derivation intro (twice),
  inversion ($\sendk$, $\recvk$), typing rules for variables and
  $\TypeEquiv$, sub-derivation elim (twice), typing rules for
  expressions as processes (twice) and parallel composition and
  weakening and copy, definition of $\dual S$, and typing rule $\nu$.

  \textbf{Case} the derivation ends with the rule for branching:
  similar to the above, but simpler.

  \textbf{Case} the derivation ends with par: inversion for parallel
  composition, induction, typing rule for parallel composition.

  \textbf{Case} the derivation ends with reduction under $\nu$:
  inversion for $\nu$, induction, typing rule for $\nu$.

  \textbf{Case} the derivation ends with $\equiv$: congruence lemma,
  induction.
\end{proof}

% \vv{In the proof above, we could spell out the details of one case
%   (there is some space left)}

We conclude this section with the results on type safety and progress
for the functional sub-language.

\begin{theorem}[Type safety]
  \label{thm:safety}
  If $\Delta;\Gamma \vdash p$, then $p$ is not an error.
\end{theorem}
%
\begin{proof}
  A simple analysis of the typing derivation for the premise. We
  analyse one of the five cases in the definition of error in
  Section~\ref{sec:semantics}, namely $(\new a,b)(E_1[e_1] \PAR
  E_2[e_2] \PAR p)$,
  where $\subj(e_1)=a$ and $\subj(e_2)=b$ and $E_1$ does not bind~$a$
  and $E_2$ does not bind~$b$. We show that $\agree^{ab}(e_1,e_2)$.
 
  The structural typing rules and those for $\newk$ and for parallel
  composition guarantee that
  $\Delta;\Gamma_1,a\colon S \vdash E_1[e_1]$ and
  $\Delta;\Gamma_2,b\colon \dual S \vdash E_2[e_2]$, for some
  $\Delta,\Gamma_1,\Gamma_2$. When $e_1$ is $\send{e'_1}a$,
  sub-derivation introduction and inversion
  (lemmas~\ref{lem:derivation-intro} and~\ref{lem:inversion}) allow to
  conclude that $\Delta \vdash S \TypeEquiv \;!B.S'$,
  hence~$\Delta \vdash \dual S \TypeEquiv \;?B.\dual{S'}$. Of all the
  terms with subject~$b$ only $\recv b$ has a type of the form
  $?B.\dual{S'}$, hence $\agree^{ab}(\send{e'_1}a,\recv b)$.
\end{proof}

\begin{corollary}[Progress for the functional sub-language]
  If $\Delta;\Gamma \vdash (\nu\vec a,\vec b)(E[e] \PAR p)$, then either
  $e\rightarrow e'$ or $e$ is a value or $e$ is of one of the
  following forms: $\send{v}{a}$, $\recv a$, $\select{l}{a}$ or
  $\case{a}{\{l_i\rightarrow e_i\}}$.  
\end{corollary}

It should be easy to see that the full language does not enjoy
progress. Consider two processes exchanging messages on two
different channels as follows.
%
\begin{equation*}
  (\nu a_1,a_2)(\nu b_1,b_2)(\sendk\;5\;a_1; \sendk\;7\;b_1 \mid \recvk\;b_2; \recvk\;a_2)
\end{equation*}
%
The nonbuffered (rendez-vous, synchronous) semantics leads to a
deadlocked situation. A recent survey reviews a few alternatives for
progress on session type
systems~\cite{huttel.lanese.etal:foundations-session-types}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Conservative Extension}
\label{sec:conservative-extension}

Our system is a conservative extension of previous session type
systems. In those systems, the session type language is restricted to
tail recursion, the $\mu$ operator works with a much simpler notion
of contractivity, and equivalence is defined modulo unfolding.
We take the definitions from the functional session type
calculus~\cite{DBLP:journals/jfp/GayV10} as a blueprint.
The first-order part of the session type language from that paper may
be defined by $S'$ in the following grammar. Henceforth, we call that
language \emph{regular session types}.
\begin{align*}
  S'_X \grmeq& \End \grmor !B.S''_X \grmor ?B.S''_X \grmor \oplus\{l_i\colon {S_i}_X''\} \grmor
         \&\{l_i\colon {S_i}_X''\}
  \\
   \grmor& \mu x. S'_{X\cup\{x\}} \\
  S''_X \grmeq& x\in X \grmor S'_X
\end{align*}

The translation $\Embed{}$ into our system is defined as follows.
\begin{align*}
  \Embed{\End} & = \skipk
  \\
  \Embed{!B.S''} & = !B; \Embed{S''}
  &
  \Embed{?B.S''} & = ?B; \Embed{S''}
  \\
  \Embed{\oplus\{l_i\colon S_i''\}} & = \oplus\{l_i\colon\Embed{S_i''}\}
  &                                      
  \Embed{\&\{l_i\colon S_i''\}} & = \&\{l_i\colon  \Embed{S_i''}\}
  \\
  \Embed{\mu x.S'} & = \mu x. \Embed{S'}
  &
  \Embed{x} & = x
\end{align*}

\begin{lemma}
  For all $S'_\emptyset$, $\cdot \vdash \Embed{S'_\emptyset} :: \kinds^\Linear$.
\end{lemma}
\begin{proof}
  We need to prove a more general property.  Define
  $\GEnv_X = x : \kinds^\Linear \PAR x \in X$ and show that for all
  $S'_X$, $\GEnv_X \vdash \Embed{S'_X} :: \kinds^\Linear$. The proof
  is by straightforward induction.
\end{proof}
\begin{lemma}
  Let $\vdash_{\text{GV}}$ be the typing judgment for expressions from
  Gay and Vasconcelos~\cite{DBLP:journals/jfp/GayV10}.  If
  $\Gamma \vdash_{\text{GV}} e : T$, then
  $\cdot; \Gamma \vdash e : \Embed{T}$.
\end{lemma}


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                             doc/ICFP2016/trace.cfs                                                                              0000664 0001750 0001750 00000000442 13216017506 014531  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                                                c ::TC
select Node      c1::!int;TC;TC;?int
send c1 x        c2::TC;TC;?int == oplus{Leaf: TC;?int, Node:!int;TC;TC;?int;TC;?int}
transform l c2   c3::TC;?int == oplus{Leaf: ?int, Node:!int;TC;TC;?int;?int}
transform r c3   c4::?int == ?int;skip
receive c4       c5::skip

                                                                                                                                                                                                                              doc/ICFP2016/conclusion.tex                                                                         0000664 0001750 0001750 00000003415 13216017506 015637  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \section{Conclusion}
\label{sec:conclusion}

Context-free session types extend the expressiveness of regular
session types by generalizing the type structure from
regular to context-free processes. This extension enables
the low-level implementation of type-safe serialization of recursive datatypes
among other examples. 

While we have established decidability of type checking, there
is still work to do towards a practical type checking
algorithm. This algorithm could be based on the algorithm that decides
BPA equivalence, but there may be other alternatives to consider \cite{DBLP:journals/iandc/LanesePSS11}.

We further believe that our approach scales to the serialization of XML
documents, but we leave the elaboration of this connection to future work.


% \vv{Shall we say something about this: ``This is a reasonable simplification for a first study of context free session
% types. There are, however, other decidability results for strong bisimulation of
% the higher-order pi calculus which could perhaps support full context free
% session types with delegation. For example:

%   I. Lanese, J. A. Pérez, D. Sangiorgi, A. Schmitt, 
%   On the expressiveness and decidability of higher-order process calculi,
%   Information and Computation 2011.''}

% \vv{Shall we say something about this: ``Does this mean that the only challenge for a type-checking algorithm is a
% practical algorithm for deciding strong bisimulation? 

% It is difficult to see an obvious answer by observing the fairly complex
% inference rules of figure 6 (the type assignment system), and the paper itself
% does not address this question. Even the challenge of finding a bisimulation
% algorithm is not mentioned before the conclusions.''}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                   doc/ICFP2016/proofs-alg-type-equivalence.tex                                                        0000664 0001750 0001750 00000012435 13216017506 021014  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{lemma}[Termination]
  \label{lem:alg-terminates}
  The type equivalence algorithm always terminates on closed session
  types.
\end{lemma}
%
\begin{proof}
  Define the set of \emph{subterms} of a session type $S$,
  $\subterms(S)$, as follows.
  \begin{equation*}
    \{S,\Unfold(S)\} \cup\left\{
      \begin{array}{ll}
        \emptyset & \text{if } \Unfold(S) = \skipk
        \\
        \{S_1\}\cup\subterms(S_2) & \text{if } \Unfold(S) = S_1;S_2
        \\
        \{S_i\mid i\in I\}  & \text{if } \Unfold(S) = \mathsf{\_}\{l_i\colon S_i\}_{i\in I}
      \end{array}
      \right.
  \end{equation*}

  Given an initial goal $\cdot\vdash S^0_1\equiv S^0_2$, let $N$ be
%  the natural number
  $\cardinality{\subterms(S^0_1)} \times
  \cardinality{\subterms(S^0_2)}$.
  % 
  Define the \emph{measure} $\measure$ of an arbitrary goal
  $\Sigma \vdash S_1 \equiv S_2$ as the pair $(N-n,m)$ where~$n$ is
  the number of assumptions in~$\Sigma$ and~$m$ is the sum of the
  nesting of type constructors in~$S_1$ and~$S_2$. Assume $\measure$
  equipped with the usual \emph{lexicographic ordering}.%
  % 
  \footnote{$(a,b)<(a',b')$ if either $a<a'$ or $a=a'$ and $b<b'$.}
  %
  It is straightforward to show that each application of a rule
  strictly decreases $\measure$.

  It remains to show that $\measure$ is well-founded; this follows
  from the fact that $N$ is finite, that $N\ge n$ (below) and that
  $m>0$ (the depth of a term is a positive number). That $N\ge n$ is
  straightforward for all rules (where~$n$ is invariant) except the
  last. For the last rule we have to show that, for each goal
  $\Sigma \vdash S_0\equiv S_1$ arising in the execution of
  $\cdot\vdash S^0_1\equiv S^0_2$, we have:
  %
  \begin{equation*}
    (S_1,S_2) \in \subterms(S_1^0) \times \subterms(S_2^0)
  \end{equation*}
  (To be completeted)
\end{proof}

Now for soundness. We again follow Gay and
Hole~\cite{DBLP:journals/acta/GayH05}.
%
Say that a goal
$S_1 \equiv S'_1,\dots,S_{n-1}\equiv S_{n-1}' \vdash S_n \equiv S_n'$
is \emph{sound} when $S_i \TypeSim S_i'$ for all $1\le i\le n$.

\begin{lemma}
  \label{lem:alg-subgoals}
  If a goal is sound then the conclusion of one of the rules in
  figure~\ref{fig:alg-type-equiv} matches the goal and the new
  subgoals corresponding to the hypotheses are all sound goals.
\end{lemma}
%
\begin{proof}
  Let $\Sigma \vdash S_1 \equiv S_2$ be a sound goal. If
  $S_1 \equiv S_2 \in \Sigma$, then the first axiom applies and there
  are no subgoals.
  
  If $S_i = \alpha;S_i'$ with $i=1,2$, we know by hypothesis that
  $\alpha;S_1' \TypeSim \alpha;S_2'$, and by definition that
  $(\alpha;S_1', \alpha;S_2') \in R$, and by unfolding that
  $(\skipk;S_1',\skipk;S_2') \in R$. The result follows from
  Lemma~\ref{lem:skip-elim}.
  %
  The cases of $S_i = \;!;S_i'$ and $S_i = \;?;S_i'$ are similar.

  In the cases for choice, soundness of the new subgoals follows from
  the definition of~$\TypeSim$.

  The case for the last rule follows from
  Lemma~\ref{lem:unfold-type-sim}.
\end{proof}

\begin{lemma}
  \label{lem:alg-not-false}
  If $S_1 \TypeSim S_2$ then the type equivalence algorithm does not
  return false when applied to $\cdot \vdash S_1 \equiv S_2$.
\end{lemma}
%
\begin{proof}
  Consider all the subgoals produced by the algorithm when given
  $\cdot\vdash S_1\equiv S_2$. From the hypothesis we know that the
  initial goal is sound; by Lemma~\ref{lem:alg-subgoals} all of the
  generated subgoals are sound.  By the same lemma, the algorithm
  either proceeds or returns true.
\end{proof}

\begin{theorem}[Soundness of algorithmic type equivalence]
  \label{thm:alg-soundness}
  If $S_1 \TypeSim S_2$ then $\cdot \vdash S_1 \equiv S_2$  
\end{theorem}
%
\begin{proof}
  By Lemma~\ref{lem:alg-terminates} the algorithm terminates. By
  Lemma~\ref{lem:alg-not-false} the algorithm does not return
  false. Therefore it must return true.
\end{proof}

Now for completeness.

\begin{lemma}
  \label{lem:unfold-preserves-alg-equiv}
  If $\cdot\vdash S_1 \equiv S_2$ then
  $\cdot\vdash \Unfold(S_1) \equiv \Unfold(S_2)$.
\end{lemma}

\begin{theorem}[Completeness of algorithmic type equivalence]
  If $\cdot\vdash S_1 \equiv S_2$ then $S_1 \TypeSim S_2$.
\end{theorem}
%
\begin{proof}
  By Lemma~\ref{lem:unfold-preserves-alg-equiv} it is sufficient to
  consider the case when $S_1$ and $S_2$ are guarded types. We show
  that
  %
  \begin{equation*}
    R = \{(S_1,S_2) \mid \cdot\vdash S_1 \equiv S_2 \text{ and } S_1
    \text{ and } S_2 \text{ are guarded}\}
  \end{equation*}
  %
  is a type simulation.

  Consider case 6 (external choice, $\&$), and assume
  $(S_1,S_2) \in R$ and
  $\Unfold(S_1) = \;\&\{l_i\colon S_i\}_{i\in I}$. Since $S_1$ is
  guarded we have $S_1=\;\&\{l_i\colon S_i\}_{i\in I}$. The only rule
  in the algorithm that applies is the \&-rule, and we get that
  $S_2 = \;\&\{l_j\colon S'_j\}_{j\in J}$. The rule ensures that $I=J$
  and that $\cdot\vdash S_i \equiv S'_i$, for all $i \in I$. Then
  Lemma~\ref{lem:unfold-preserves-alg-equiv} ensures that
  $\cdot\vdash \Unfold(S_i) \equiv \Unfold(S'_i)$, and
  Lemma~\ref{lem:unfold-yields-guarded-types} that $S_i$ and $S'_i$
  are guarded, hence $(S_i',S_i') \in R$ as required.  
\end{proof}

\begin{corollary}
  $\cdot \vdash S_1 \equiv S_2$ if and only if $\S_1 \TypeSim S_2$.
\end{corollary}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                   doc/ICFP2016/fig-translate-bpa.tex                                                                  0000664 0001750 0001750 00000003720 13216017506 016762  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[tp]
    \begin{align*}
      \toLHS (\skipk) & = \Return\ \varepsilon \\
      \toLHS (A) & = \Return\ A \\
      \toLHS (S_1; S_2) & = \Do
                              \begin{array}[t]{l}
                                E_1 \gets \toLHS (S_1) \\
                                E_2 \gets \toLHS (S_2) \\
                                \Return (E_1\fatsemi E_2)
                              \end{array}
      \\
      \toLHS (\star\{\overline{l_n:S_n}\}) &= \Do
                                                 \begin{array}[t]{l}
                                                   \dots \\
                                                   E_i \gets \toLHS (S_i) \\
                                                   \dots \\
                                                   \Return\ (\star l_1\fatsemi E_1 + \dots + \star l_n\fatsemi E_n)
                                                 \end{array}
      \\
      \toLHS (\mu x.S) &= \Do
                             \begin{array}[t]{l}
                               E \gets \toLHS (S) \\
                               \Out (x = E) \\
                               \Return\ x
                             \end{array}
      \\
      \toLHS (x) &= \Return\ x
      \\
      \toBPA{S} &= \Do
                   \begin{array}[t]{l}
                     E \gets \toLHS (S) \\
                     \Out (x_0 = E) \\
                     \Return\ x_0
                   \end{array}
    \end{align*}
    Here $\fatsemi$ is defined as a smart sequencing constructor.
    \begin{align*}
      E_1 \fatsemi E_2 &=
                         \begin{cases}
                           E_1 & E_2=\varepsilon \\
                           E_2 & E_1=\varepsilon \\
                           E_1;E_2 & \text{otherwise}
                         \end{cases}
    \end{align*}
    \caption{Translation of a session type to a BPA}
    \label{fig:session-to-bpa}
  \end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                doc/ICFP2016/An_Introduction_to_Bisimulation_and_Coin.bibtex                                        0000664 0001750 0001750 00000000411 13216017506 024243  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               @book{sangiorgi2014introduction,
  title={An Introduction to Bisimulation and Coinduction},
  author={Sangiorgi, Davide},
  isbn={9781139161381},
  ignoreurl={https://books.google.de/books?id=PIjWoQEACAAJ},
  year={2014},
  publisher={Cambridge University Press}
}
                                                                                                                                                                                                                                                       doc/ICFP2016/fig-type-equivalence-lifted.tex                                                        0000664 0001750 0001750 00000002017 13216017506 020750  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[tp]
  \begin{gather*}
    \frac
    {T \TypeEquiv T'}
    { \forall \alpha.T \TypeEquiv \forall \alpha. T'}
    \qquad
    \alpha \TypeEquiv \alpha
    \qquad
    B \TypeEquiv B
    \\
    \frac
    { T_1 \TypeEquiv T_1' \quad T_2 \TypeEquiv T_2'}
    {T_1 \to T_2 \TypeEquiv T_1' \to T_2'}
    \qquad
    \frac
    { T_1 \TypeEquiv T_1' \quad T_2 \TypeEquiv T_2'}
    { T_1 \multimap T_2 \TypeEquiv T_1' \multimap T_2'}
    \\
    \frac
    {T_1 \TypeEquiv T_1' \quad T_2 \TypeEquiv T_2'}
    { T_1 \otimes T_2 \TypeEquiv T_1' \otimes T_2'}
    \qquad
    \frac
    {(\forall i\in I)~ T_i \TypeEquiv T_i'}
    { [l_i:T_i]_{i\in I} \TypeEquiv [l_i:T_i']_{i\in I}}
    \\
    \frac
    { T\subs{\mu x.T}x \TypeEquiv T'}
    { \mu x.T \TypeEquiv T'}
    \qquad
    \frac
    { T \TypeEquiv T'\subs{\mu x.T'}x}
    { T \TypeEquiv \mu x.T'}
  \end{gather*}
  \caption{Type equivalence lifted to all types}
  \label{fig:type-equivalence-lifted}
\end{figure}


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 doc/ICFP2016/treechannel.cfs                                                                        0000664 0001750 0001750 00000000116 13216017506 015721  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               type TreeChannel = oplus{
  Leaf: skip,
  Node: !int;TreeChannel;TreeChannel}
                                                                                                                                                                                                                                                                                                                                                                                                                                                  doc/ICFP2016/appendix.tex                                                                           0000664 0001750 0001750 00000016551 13216017506 015300  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \appendix
\section{Transforming recursive types}
\label{sec:transf-recurs-types}


\begin{figure}[t]
  \begin{align*}
    a & ::= \omega && \text{infinite words, only} \\
    & \mid \infty && \text{finite and infinite words} \\
    & \omega \sqsubset \infty \\
    n & ::= 0 \mid 1 && \text{minimum length of trace} \\
    A & ::= \cdot \mid A, x:a && \text{environments} \\
    C & ::= \cdot \mid C, (x,n) && \text{depth of guardedness}
  \end{align*}
  \begin{gather*}
    \frac{}{A \vdash \skipk : \infty; 0; C }
    \quad
    \frac{}{A \vdash {!B} : \infty; 1; C}
    \quad
    \frac{}{A \vdash {?B} : \infty; 1; C}
    \\
    \frac{A \vdash S_1 : a_1;n_1;C_1 \quad A \vdash S_2 : a_2;n_2;C_2}{
      A \vdash (S_1;S_2) : a_1 \sqcap a_2;\seq(a_1)(a_2)}
    \\
    \frac{(\forall i\in I)~A \vdash S_i : a_i; n_i ; C_i}{
      A \vdash \star\{l_i\colon S_i\}_{i\in I} :
      \bigsqcup_i a_i; 
      1; \bigsqcap_i C_i \uparrow 1}
%     \quad
%     \frac{(\forall i\in I)~A \vdash S_i : a_i}{
%       A \vdash \&\{l_i\colon S_i\}_{i\in I} :
%       \bigsqcup_i a_i}
    \\
    \frac{}{A, x : a \vdash x : 1 ; C,(x,0)} \quad
    \frac{A, x : a \vdash S : a; n ; C, (x,1) }{A \vdash \mu x.S : a; n; C}
  \end{gather*}
  \begin{align*}
    \seq (n_1; C_1) (n_2; C_2) & = (n_1 + n_2 ; C_1 \sqcap C_2 \uparrow n_1
    ) \\
    (x, i) \uparrow n &= (x, i + n)
  \end{align*}
  \caption{Inference for infiniteness (and contractivity)}
  \label{fig:inference-infiniteness}
\end{figure}
Figure~\ref{fig:inference-infiniteness} contains an inference system for detecting finite words. It
classifies session types $S$ according to their trace languages. The intent is as follows: If all traces of $S$ are infinite
words in $\Sigma^\omega$, then $A \vdash S : \omega; n; C$ is derivable. If $S$ may admit a finite trace
in $\Sigma^\infty$, then $A \vdash S : \infty; n; C$ is derivable. The result $n$ indicates the
minimum length of a trace generated/accepted by $S$. The $C$ contains pairs of the form $(x,n)$
which indicates a lower bound on the trace produced before $x$ is mentioned recursively. The
formation rule for $\mu x.S$ requires $(x,1)$ which is equivalent to contractivity in $x$.


We write $A \models \rho$ if $\dom (A) = \dom (\rho)$ and for all $x :a \in A$, let $R_x = \rho (x)$
where $R_x \ne \emptyset$, $R_x \subseteq \Sigma^a$, and $\min\{ |t| \mid t\in R_x \}\ge1$.

\begin{lemma}
  If $A \vdash S : a; n; C$ and $A \models \rho$, then $T = \TR (S) \rho \subseteq \Sigma^a$ and
  $\min\{ |t| \mid t \in T\} \ge n$.
\end{lemma}
\begin{proof}
  Induction on $S$. Relies on $\TR (S) \rho \ne \emptyset$, for all $S$.

  \textbf{Case }$\skipk$, $!B$, $?B$: Immediate.

  \textbf{Case }$(S_1;S_2)$: By inversion, $A \vdash S_1: a_1;n_1;C_1$, $A\vdash S_2:a_2;n_2;C_2$, and $a = a_1
  \sqcap a_2$, $(n;C) = \seq (n_1;C_1) (n_2;C_2)$.
  If $a=\omega$, then there are two non-exclusive cases $a_1=\omega$ or $a_2 = \omega$.

  \textbf{Subcase }$a_1 = \omega$: By induction $\TR (S_1) \rho \subseteq \Sigma^\omega$, hence $\TR
  (S_1;S_2)\rho = \TR (S_1)\rho \cdot \TR (S_2)\rho = \TR (S_1)\rho \subseteq \Sigma^\omega$ because $\TR (S_2)\rho$
  is not empty. The condition on $n$ holds trivially because there are no finite traces.

  \textbf{Subcase }$a_1 = \infty$ and $a_2 = \omega$: By induction $\TR (S_2) \rho \subseteq \Sigma^\omega$, hence  $\TR
  (S_1;S_2)\rho = \TR (S_1)\rho \cdot \TR (S_2)\rho \subseteq \Sigma^\omega$ because $\TR (S_1)\rho$
  is not empty. Again, the condition on $n$ holds trivially.

  \textbf{Subcase }$a_1 = a_2 = \omega$: The condition on $n$ holds because $|v\cdot w| = |v|+|w|$.

  \textbf{Case }$\star\{l_i\colon S_i\}$: By inversion, $A \vdash S_i : a_i; n_i; C_i$ and $a = \bigsqcup_i
  a_i$, $n=1$, and $C = \bigsqcap_i C_i \uparrow 1$.
  If $a=\omega$, then $a_i = \omega$, for all $i$. Hence, by induction $\TR (\oplus\{l_i\colon
  S_i\})\rho = \bigcup_i \{L_i\}\cdot\TR (S_i)\rho \subseteq \Sigma^\omega$. The condition on $n$ is
  trivial because each trace has length $\ge1$.

  \textbf{Case }$x$: Immediate by assumption $A \models \rho$.

  \textbf{Case }$\mu x.S$: By inversion, $A, x:a \vdash S:a;n;C,(x,1)$. If $a=\infty$, then any (non-empty)
  extension of $\rho$ satisfies $A, x:\infty \models \rho[x\mapsto Y]$ and the result is immediate by
  induction. 

  If $a= \omega$, then consider
  \begin{align*}
    \TR (\mu x.S)\rho & = \GFP\, \lambda Y. \TR (S)\rho[x\mapsto Y] \\
    & = \TR (S)\rho[x\mapsto \GFP\, \lambda Y. \TR (S)\rho[x\mapsto Y]]
  \end{align*}

  \textbf{Subsidiary lemma:}
  Let $F (Y) = \TR (S)\rho[x\mapsto Y]$ and show that an $F$-consistent set
  cannot contain finite words.

  That is, suppose that $Y \subseteq \TR (S)\rho[x\mapsto Y]$ and there exists some $v \in Y$ of 
  length $|v| = k < \infty$. We show that there must be some $v' \in Y$ with $|v'|\le k-m$ where
  $(x,m)$ is the minimal prefix length for $x$ in $C$.

  Given that result, we argue as follows. Choose $v \in Y$ of finite minimal length $k$. From the
  inversion, we know that $m=1$. Hence, there exists some $v' \in Y$ with length $\le k-1$. From
  this contradiction it follows that $Y$ contains no finite elements.
  
  Proof by induction on the derivation of $A, x:\omega \vdash S : \omega; n; C,(x,m)$.

  \textbf{Case }$\skipk$, $!B$, $?B$: contradiction because they do not derive $\omega$.

  \textbf{Case }$(S_1;S_2)$: Observe that $v \in \TR (S_1;S_2)\rho[x\mapsto Y]$ implies that $v = v_1v_2$ with
  $v_1 \in \TR (S_1)\rho[x\mapsto Y]$ and $v_2 \in \TR (S_2)\rho[x\mapsto Y]$ where $|v_1|, |v_2| \le |v| \le k$.
  By inversion, there are two subcases:

  \textbf{Subcase }$A \vdash S_1:\omega;n_1;C_1,(x,m_1)$ where $m_1\sqsupseteq m$: By induction,
  there exists some $v'\in Y$ with $|v'| \le |v_1| - m_1 \le |v| - m$.

  \textbf{Subcase }$A \vdash S_1:\infty;n_1;C_1,(x,m_1)$ and $A \vdash S_2:\omega;n_2;C_2,(x,m_2)$
  where  $m_1\sqsupseteq m$ and  $m_2 + n_1 \sqsupseteq m$:
  By induction, there exists some $v'\in Y$ with $|v'| \le  |v_2| - m_2 \le |v| - m$ (if $n_1=0$).
  If $n_1=1$, then $|v_2| < |v|$ and we can exploit the inductive hypothesis similarly.

  \textbf{Case }$\star\{l_i\colon S_i\}$: Inversion yields $A \vdash S_i: \omega; n_i;C_i,(x,m_i)$
  and $m =1$.
  In this case, $v = L_iv_i$ (for some $i$) and $v_i \in \TR
  (S_i)\rho[x\mapsto Y]$. By induction, there exists $v'\in Y$ with $|v'|\le |v_i| - m_i \le |v_i|+1
  -1 = |v| -1$.

  \textbf{Case }$x$: In this case, $m=0$ and $v \in \rho (x) = Y$ and $|v|\le |v|-0$.

  \textbf{Case }$x'\ne x$: By inversion, it must be that $x':\omega \in A$. By $A \models \rho$, it
  must be that $v \in \rho (A) \subseteq \Sigma^\omega$, a contradiction.

  \textbf{Case }$\mu x'.S$: By the outer induction, this implies that $v\in\Sigma^\omega$, a contradiction.
\end{proof}

\begin{lemma}
  For each $S$, there is a minimal derivation, where derivations are ordered pointwise on judgments;
  judgments are ordered pointwise on environments $A$ and the $a$-component of the result.
\end{lemma}

\begin{lemma}
  If $A \vdash (S_1; S_2) : \omega; n; C$ is derivable with subderivation $A \vdash S_1 : \omega;
  n_1; C_1$, then $(S_1;S_2) \TypeEquiv S_1$.
\end{lemma}

This lemma enables the direct syntactic transformation of $\mu x.{!B};x;x$ to the equivalent $\mu
x.{!B}; x$. It even applies to proving $\mu x.{!B}; x; S \TypeEquiv \mu x.{!B}; x$ for any $S$.


\section{Obsolete type stuff}
\input{type-simulation}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                       doc/ICFP2016/related.tex                                                                            0000664 0001750 0001750 00000007030 13216017506 015100  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \section{Related Work}
\label{sec:related-work}

The system we propose is ultimately rooted in the work of Honda et
al.\ on session types~\cite{DBLP:conf/concur/Honda93,DBLP:conf/parle/TakeuchiHK94,DBLP:conf/esop/HondaVK98}.
%
The particular language of this paper is closely related to the one
proposed by Gay and Vasconcelos~\cite{DBLP:journals/jfp/GayV10}. The
main difference is at the level of semantics: we use a synchronous
semantics in place of a buffered one. We make this choice to simplify
the technical treatment of the operational semantics. We believe that
a buffered semantics can be derived without compromising the most
important properties of the language. At the level of the language,
and in addition to Gay and Vasconcelos, we incorporate variant types
and recursion on functional types. The linear treatment of session
types is identical, including the syntactic distinction of the
two ends of a channel, related by a $\nu$-binding.

The predicative polymorphism we employ is closely related to that of
Bono et al.~\cite{BonoPadovaniTosatto13}, including the kinding system
for type variables. The extra complexity of context-free types lead us
to a more elaborate kinding system, allowing to distinguish session (or
end point) types, from functional types and type schemes (Bono et al.\
rely on different syntactic categories).
%
% Predicative polymorphism for the $\pi$-calculus was introduced by
% Vasconcelos~\cite{DBLP:conf/parle/Vasconcelos94}.
%
A different form of polymorphism---bounded polymorphism on the values
transmitted on channels---was introduced by
Gay~\cite{DBLP:journals/mscs/Gay08} in the realm of session types for
the $\pi$-calculus.

Wadler~\cite{DBLP:journals/jfp/Wadler14} gives a typing preserving
translation of the Gay and Vasconcelos calculus mentioned before to a process
calculus inspired by the work of Caires and
Pfenning~\cite{DBLP:conf/concur/CairesP10}.  The semantics of these
systems, given directly by the cut elimination rules of linear logic,
ensure deadlock freedom. Even though our system ensures progress for
the functional part of the language, the unrestricted interleaving of
channel read/write on multiple channels may lead to deadlocked situations. That is the
price to pay for the flexibility our language offers with respect to
the work of Caires, Pfenning, and Wadler~\cite{DBLP:conf/concur/CairesP10,DBLP:journals/jfp/Wadler14}.

% The Sill language described by Toninho, Caires, and
% Pfenning~\cite{DBLP:conf/esop/ToninhoCP13} 
Functional languages with conventional session
types~\cite{DBLP:journals/jfp/GayV10,DBLP:conf/esop/ToninhoCP13} can
describe type-safe protocols to transmit trees. Doing so
requires a higher-order recursive session type of the following shape:
\begin{lstlisting}
TreeC = oplus{Leaf: end, Node: !int.!TreeC.!TreeC}
\end{lstlisting}
That is, to transmit a node \lstinline|Node(i,t1,t2)| on channel $c$, the originating process first sends the
integer \lstinline|i| on $c$. But then it creates two new channels $c_1$ and $c_2$,
sends their receiving ends on $c$, and closes $c$. Finally, it
recursively transmits \lstinline|t1| on $c_1$ and \lstinline|t2| on
$c_2$.
In comparison, our calculus is intentionally closer to a low level
language: it only supports the transmission of base type values. We
furthermore believe that its run-time implementation is simpler and
more efficient: only one channel is created and used for the
transmission of the tree; thus, it avoids the overhead of multiple
channel creation and channel passing.

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        doc/ICFP2016/arithmetic-server-data.cfs                                                             0000664 0001750 0001750 00000001444 13216017506 020002  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               data TermData = Const int | Add TermData TermData | Mult TermData TermData

sendTerm :: forallalpha.TermData -o TermChannel;alpha -o alpha
sendTerm (Const n) =
  let c1 = select Const c
  in send n c1
sendTerm (Add t1 t2) =
  let c1 = select Add c
      c2 = sendTerm t1 c1
  in sendTerm t2 c2
sendTerm (AddMult t1 t2) =
  let c1 = select Mult c
      c2 = sendTerm t1 c1
  in sendTerm t2 c2

computeClient :: TermData -o TermChannel;?int -o int otimes skip
computeClient = receive . sendTerm

aTerm = Add (Const 5) (Mult (Const 7) (Const 9)) 
go :: int otimes skip
go =
  let c s = new TermChannel;?int
  in fork (computeService s);
     computeClient aTerm c

{- Exercise: write functions receiveTerm and eval in such a way that: -}

computeService c =
  let t c1 = receiveTerm c
  in send (eval t) c1
                                                                                                                                                                                                                            doc/ICFP2016/regular.tex                                                                            0000664 0001750 0001750 00000010767 13216017506 015134  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \documentclass[11pt]{article}
\usepackage{amsmath}
\usepackage{stmaryrd}

\newcommand\Sem[1]{\llbracket#1\rrbracket}
\newcommand\LFP{\ensuremath{\mathsf{lfp}}}
\newcommand\SUB{\ensuremath{\mathsf{sub}}}
\newcommand\FREE{\ensuremath{\mathsf{free}}}

\begin{document}
Define $\mu$-regular expressions, $a\in T$ a terminal symbol
\begin{displaymath}
  r,s ::= 0 \mid 1 \mid a \mid r+s \mid r.s \mid \mu x.r \mid x
\end{displaymath}
Semantics of a $\mu$-regular expression.
\begin{align*}
  \Sem{0}\rho &= \emptyset\\
  \Sem{1}\rho &= \{ \varepsilon \}\\
  \Sem{a}\rho &= \{ a \} \\
  \Sem{r+s}\rho &= \Sem{r}\rho \cup \Sem{s}\rho \\
  \Sem{r.s}\rho &= \Sem{r}\rho \cdot \Sem{s}\rho \\
  \Sem{\mu x. r}\rho &= \LFP\, \lambda X. \Sem{r}\rho[x \mapsto X]\\
  \Sem{x}\rho &=\rho(x) 
\end{align*}
Alternative semantics that relies on substitution of symbols by languages.
\begin{align*}
  \Sem{0} &= \emptyset\\
  \Sem{1} &= \{ \varepsilon \}\\
  \Sem{a} &= \{ a \} \\
  \Sem{r+s} &= \Sem{r} \cup \Sem{s} \\
  \Sem{r.s} &= \Sem{r} \cdot \Sem{s} \\
  \Sem{\mu x. r} &= \LFP\, \lambda X. \SUB(x, X, \Sem{r})\\
  \Sem{x}\rho &= \{ x\}
\end{align*}

Define a grammar $G= (N, T, P, S)$ from a $\mu$-regular expression $r$.
Assume that each variable $x$ is bound at most once by a $\mu$ and that $r$ is closed.
\begin{itemize}
\item $N = \{ [s] \mid s \text{ subterm of }r \}$
\item $S = [r]$
\item The set of productions $P$ is given as follows
  \begin{itemize}
  \item no production for nonterminal $[0]$
  \item $[1] \to \varepsilon$
  \item $[a] \to a$, $a\in T$
  \item $[r+s] \to [r] \mid [s]$
  \item $[r.s] \to [r][s]$
  \item $[\mu x.r] \to [r]$
  \item $[x] \to [r]$ where $\mu x.r$ is the unique binding for $x$
  \end{itemize}
\end{itemize}

Let $V = \{ [x] \mid x \in \FREE (s) \}$.
Show that $L (G, [s]) = \Sem{s}$ where we shift $V$ from $N$ to $T$.

Induction on $s$.

\textbf{Case }$0$: $L (G, [0]) = \emptyset = \Sem{0}$.

\textbf{Case }$1$: $L (G, [1]) = \{\varepsilon \} = \Sem{1}$.

\textbf{Case }$a$: $L (G, [a]) = \{ a \} = \Sem{a}$.

\textbf{Case }$r+s$: $L (G, [r+s]) = L (G, [r]) \cup L (G, [s]) = \Sem{r} \cup \Sem{s} = \Sem{r+s}$.

\textbf{Case }$r.s$: $L (G, [r.s]) = L (G, [r]) \cdot L (G, [s]) = \Sem{r} \cdot \Sem{s} = \Sem{r.s}$.

\textbf{Case }$x$: $L (G_V, [x]) = \{ x \} = \Sem{x}$.

\textbf{Case }$\mu x.r$: Let $W = V \cup \{x\}$
\begin{align*}
  L (G_V, [\mu x.r])
  & = L (G_V, [r])\\
  & = \SUB (x, L (G_V, [\mu x.r]), L (G_W, [r])) \\
  & ??? \\
  &= \SUB(x,\Sem{\mu x.r}, \Sem{r}) \\
  &= \SUB(x, \LFP\, \lambda X. \SUB(x, X, \Sem{r}), \Sem{r}) \\
  &= \LFP\, \lambda X. \SUB(x, X, \Sem{r}) \\
  &=  \Sem{\mu x. r}
\end{align*}
Fixpoint induction
\begin{align*}
  \LFP\, \lambda X. \SUB(x, X, \Sem{r}) &\subseteq   L (G_V, [\mu x.r])\\
                                        & \text{if} \\
  \SUB (x, L (G_V, [\mu x.r]), \Sem{r}) & \subseteq L (G_V, [\mu x.r]) \\
                                        &= L (G_V, [r])\\
                                        & = \SUB (x, L (G_V, [\mu x.r]), L (G_W, [r])) \\
                                        & \stackrel{IH}{=} \SUB (x, L (G_V, [\mu x.r]), \Sem{r})
\end{align*}

\clearpage{}
A different approach. Consider a context-free language defined by a system of equations with $\mu$-regular expressions on the right hand sides. Its semantics relies on the environment semantics for expressions.
\begin{align*}
  \mathcal{G} &= \{ x_i = r_i \mid 1\le i\le n \} \\
  \Sem{\mathcal{G}} &= \LFP\ \lambda \rho. [x_i \mapsto \Sem{r_j}\rho]
\end{align*}

Now suppose that $r_j = R[\mu x.r_0]$ where $R$ is a $\mu$-free context and (wlog) $x=x_0$ is different from all $a_i$, $1\le i\le n$. If we introduce a new equation $x_0 = \mu x_0. r_0$, this addition does not change the languages generated in the variables $x_1, \dots, x_n$ because their right hand sides have no free occurrences of $x_0$.

The theorem of Bekic-Leszczylowski is applicable to the augmented system of equations because $\mu$ is interpreted as the least fixpoint. Its application results in a grammar $\mathcal{G}'$ defined by
\begin{align*}
  x_0 & = r_0 \\
  x_1 & = r_1 \\
      & \vdots \\
  x_j & = R[x_0] \\
      & \vdots \\
  x_n & = r_n
\end{align*}
By Bekic-Leszczylowski, $\Sem{\mathcal{G}} (x_i) = \Sem{\mathcal{G}'} (x_i)$, for $1\le i \le n$.

By induction on the number of $\mu$-operators in the original system of equations, we obtain a system which is equivalent to the original with respect to projection on $x_1, \dots, x_n$ and where the right hand sides contain no $\mu$-operators, i.e., a context-free grammar. 

\end{document}
         doc/ICFP2016/type-simulation.tex                                                                    0000664 0001750 0001750 00000077761 13216017506 016645  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \subsection{Type unfolding}
\label{sec:type-unfolding}

We establish a notion of type unfolding as a step towards
defining type equivalence. The idea of unfolding is to expose the
first nontrivial action of a session type by squeezing out sequences
of $\skipk$s.
Let $A$ range over $\alpha$, $!B$, and $?B$; let $\star$ range over
$\oplus$ and $\&$. 
%
Define the unfolding of a type $T$,  $\Unfold(T)$, recursively by cases on the
structure of~$T$ as follows. 
\begin{enumerate}
\item $\Unfold(\mu x.T) = \Unfold(T\subs{\mu x.T}x)$
\item $\Unfold (S;S') = \left\{%
  \begin{array}{ll}
    \Unfold(S') & \Unfold(S) = \skipk
    \\
    (A; \Norm(S')) & \Unfold(S) = A
    \\
    (S_3; (S_4\fatsemi S')) & \Unfold(S) = (S_3;S_4)
    \\
    \star\{l_i\colon S_i\fatsemi S'\}  & \Unfold (S) = \star\{l_i\colon S_i\}
  \end{array}
  \right.
$
\item $\Unfold(T) = T$, otherwise
\end{enumerate}
We assume two auxiliary definitions
\begin{align*}
  \Norm (S) &=
              \begin{cases}
                S_1 \fatsemi S_2 & S = (S_1; S_2) \\
                S & \text{otherwise}
              \end{cases}
  \\
  S_1 \fatsemi S_2 &=
                     \begin{cases}
                       S_1' \fatsemi (S_1'' \fatsemi S_2) & S_1 =
                       (S_1'; S_1'') \\
                       (S_1; S_2) & \text{otherwise}
                     \end{cases}
\end{align*}

The function $\Unfold$ is well-defined and terminating because we
assume that the body of a recursive type is contractive. The auxiliary
functions $\Norm$ and $\fatsemi$ are both terminating.
The following
lemmas establish well-definedness of $\Unfold$.

\begin{lemma}\label{lemma:app:guarded=skip}
  Suppose that $\GEnv$ does not bind recursion variables and that
  $\sigma$ is a substitution of recursion variables by recursive types.
  If $\GEnv \Contr S : \Guarded$, then $\Unfold (S\sigma) = \skipk$.
\end{lemma}
\begin{proof}
  Induction on $\GEnv \Contr S : \Guarded$.

  \textbf{Case }$\GEnv \Contr \skipk : \Guarded$. Immediate.

  \textbf{Case }$\GEnv \Contr (S_1;S_2) : \Guarded$ because $\GEnv
  \Contr S_1 : \Guarded$ and $\GEnv \Contr S_2 : \Guarded$. By
  induction, $\Unfold (S_1\sigma) = \skipk$, hence $\Unfold ((S_1;S_2)\sigma) =
  \Unfold (S_2\sigma) = \skipk$ by induction.

  \textbf{Case }$\GEnv \Contr \mu x.S : \Guarded$ because $\GEnv
  \Contr S : \Guarded$. In this case, $\Unfold ((\mu x.S)\sigma) = \Unfold
  (S[\mu x.S/x]\sigma) = \skipk$ by induction (for $\sigma' = [\mu x.S/x]\sigma$).
\end{proof}

\begin{definition}
  A \emph{guarded} type has one of the forms below.
  % where $A$ ranges over $\alpha$, $!B$, and $?B$.
\begin{gather*}
  \skipk \quad A \quad A;S' \quad
  \star\{l_i\colon S_i\} %\quad \&\{l_i\colon S_i\}
  \\
  T \to T' \quad T \multimap T' \quad T \otimes T' \quad [l_i : T_i]
  \quad B 
\end{gather*}
\end{definition}
% We say that %
% %
% \footnote{To me $T$ is only a type if $\Theta \vdash T \isOk$ holds,
%   for some $\Theta$ that does not bind recursion variables. To
%   $(B\rightarrow B;\skipk)$ I call a piece of \emph{junk syntax}. This
%   means that ``there is no life outside types'' (the intuitionistic
%   approach). Pragmatically, it means that, whenever we talk of $T$, we
%   need not keep saying ``s.t.\ $\Theta \vdash T \isOk$ for some
%   $\Theta$ that does not bind recursion variables.''. In this , a
%   session type $S$ is a type such that $\Delta \vdash S: \kinds^m$,
%   for some $\Delta$ that does not bind recursion variables.}
% %


\begin{lemma}[Characterization of $\Unfold$]
  \label{lem:app:unfold-yields-guarded-types}
  Suppose that $\GEnv$ does not bind recursion variables and that
  $\GEnv \vdash T :: \kind$ for $\kind \le \kindt^\Unrestricted$, then $\Unfold (T)$ is defined and yields a
  guarded type.

  % result that has one of the following forms.
  % \begin{enumerate}
  % \item $ \skipk$,
  % \item $(\alpha;S')$, $(!B; S')$, $(?B; S')$ for some $\Delta \vdash S' \isOk$,
  % \item $\oplus\{l_i\colon S_i\}$,  $\&\{l_i\colon S_i\}$ for some
  %   $\Delta \vdash S_i \isOk$.
  % \end{enumerate}

  % Furthermore, if $\GEnv \Contr S : \gamma$, then $\Unfold (\mu
  % x.S)$ is defined and yields a guarded type.
\end{lemma}
\begin{proof}
  Induction on the derivation of  $\GEnv \vdash T :: \kind$.

  \textbf{Case }$\GEnv \vdash \skipk :: \kinds^\Unrestricted$.
  In this case, $\Unfold (\skipk) = \skipk$.

  \textbf{Case }$\GEnv \vdash A :: \kind$. $\Unfold (A) = {A}$.

  % \textbf{Case }$\GEnv \vdash !B :: \kinds^\Linear$. $\Unfold (!B) = {!B}$.
  % \textbf{Case }$\GEnv \vdash ?B :: \kinds^\Linear$. $\Unfold (?B) = {?B}$.

  \textbf{Case }$\GEnv \vdash (S_1;S_2) :: \kinds^m$.

  Inversion yields $\GEnv \vdash S_1 :: \kinds^{m_1}$ and  $\GEnv
  \vdash S_2 :: \kinds^{m_2}$. By induction, $S_1' = \Unfold (S_1)$ is
  guarded.

  \textbf{Subcase }$S_1' = \skipk$. In this case, $\Unfold (S_1;S_2) =
  \Unfold (S_2)$ which is guarded by induction.

  \textbf{Subcase }$S_1' = A$. Then $\Unfold (S_1;S_2) = (A; S_2)$
  which is guarded.

  \textbf{Subcase }$S_1' = (A; S_3)$. Hence, $\Unfold (S_1;S_2) = (A; (S_3; S_2))$ is guarded.

  \textbf{Subcase }$S_1' = \star\{l_i\colon S_i\}$: $\Unfold
  (S_1;S_2) = \star\{l_i\colon (S_i; S_2) \}$.

  % \textbf{Subcase }$S_1' = \&\{l_i\colon S_i\}$: $\Unfold
  % (S_1;S_2) = \&\{l_i\colon (S_i; S_2) \}$.

  \textbf{Case }$\GEnv \vdash \star\{l_i\colon S_i\}
  :: \kinds^\Linear$. Immediate.

  % \textbf{Case }$\GEnv \vdash \&\{l_i\colon S_i\}
  % :: \kinds^\Linear$. Immediate.

  % \textbf{Case }$\GEnv \vdash \alpha :: \kind$. Immediate: $\Unfold
  % (\alpha) = \alpha$.

  \textbf{Case }$\GEnv \vdash \mu x. T :: \kind$.
%
  Inversion yields $\GEnv, x:\kind \vdash T :: \kind$ and
  $\GEnv \Contr T : \gamma$. Observe that
  $\Unfold (\mu x.T) = \Unfold (T[\mu x.T/x])$ and
  $\GEnv \Contr T[\mu x.T/x] : \gamma$. By the second claim 
  $\Unfold (T[\mu x.T/x])$ is defined and yields a guarded type using
  $\sigma = [\mu x.T/x]$.

  \textbf{All other cases}: $\Unfold (T) = T$ is guarded.
  
  \textbf{Second claim.}
  % It holds that $\Unfold (\mu x.S) = \Unfold (S[\mu x.S/x])$.
  % Further $\GEnv\setminus x \Contr S : \gamma$ implies that
  % $\GEnv \Contr S[\mu x.S/x] : \gamma$.
  Suppose that  $\GEnv \Contr T :  \gamma$ where $\GEnv$ does not bind
  recursion variables and that  $\sigma$ is a substitution on recursion variables.
  Then  $\Unfold (T\sigma)$ is defined and yields a guarded type.

  The proof is by induction on the derivation of $\GEnv \Contr T :
  \gamma$.

  \textbf{Case }$\skipk$. Immediate.

  \textbf{Case }$A \in \{ {!B}, {?B}, \alpha\}$. $\Unfold (A\sigma) = A$ which is guarded.

  \textbf{Case }$x$ cannot occur because $\GEnv$ 
  contains no assumptions about recursion variables.

  \textbf{Case }$\star\{l_i\colon S_i\}$. $\Unfold
  ((\star\{l_i\colon S_i\})\sigma) = \Unfold (\star\{l_i\colon
  S_i\sigma\}) = \star\{l_i\colon S_i\sigma\}$ which is guarded.

  % \textbf{Case }$\&\{l_i\colon S_i\}$. Analogously.

  \textbf{Case }$(S_1;S_2) : \Productive$ because $S_1 :
  \Productive$. By induction $S_1' = \Unfold (S_1)$ is
  guarded. Proceed by subcases on $S_1'$.

  \textbf{Subcase }$\skipk$. Contradicts $S_1 : \Productive$.

  \textbf{Subcase }$A$. Here, $\Unfold ((S_1;S_2)\sigma) = (A;
  S_1)\sigma$, which is guarded.

  \textbf{Subcase }$(A; S_3)$. $\Unfold
  ((S_1;S_2)\sigma) = (A; (S_3; S_2))\sigma$, which is guarded. 

  \textbf{Subcase }$\star\{l_i\colon S_i\}$. $\Unfold
  ((S_1;S_2)\sigma) = \star\{l_i\colon (S_i; S_2)\sigma\}$ is guarded.

  % \textbf{Subcase }$\&\{l_i\colon S_i\}$. Similar.

  \textbf{Case }$(S_1;S_2) : \Productive$ because $S_1 :
  \Guarded$ and $S_2 : \Productive$. In this case, $\Unfold (S_1) =
  \skipk$ by Lemma~\ref{lemma:guarded=skip} so that the result is
  $\Unfold (S_2\sigma)$, which is guarded by induction on $S_2 : \Productive$.

  \textbf{Case }$\GEnv \Contr \mu x.T : \gamma$ because $\GEnv \Contr
  T : \gamma$. Hence, $\Unfold ((\mu x.T)\sigma) = \Unfold (T\sigma[\mu
  x.T\sigma/x])$. The result follows by induction using $\sigma' = \sigma[\mu
  x.T\sigma/x]$.

  % \textbf{Case }$\GEnv, \alpha : \gamma \Contr \alpha :
  % \gamma$. Immediate.

  \textbf{All remaining cases}: Immediate.
\end{proof}

Next, we consider invariance of kinding and contractivity under
unfolding of recursion anywhere in the type.

\begin{lemma}[Weakening]\label{lemma:app:weakening-kind}
  If $\Delta \vdash T :: \kind$, then
  $\Delta, x\colon \gamma \vdash T :: \kind$ for some $x$ not in
  $\Delta$.
\end{lemma}


\begin{lemma}[Unfolding preserves kinding]
  If $\Delta \vdash T :: \kind$ then $\Delta \vdash \Unfold(T) :: \kind$.
\end{lemma}
%
\begin{proof}
  We only consider the case for a recursive type as the other cases
  are straightforward.
  
  % (Needs adjustment)
  If  $\GEnv \vdash \mu x.T :: \kinds^m$, it must be because  $\GEnv,
  x:\kinds^m \vdash T :: \kinds^m$ and $\GEnv \Contr T : \gamma$.
  We prove by induction on $\GEnv, x:\kinds^m \vdash T :: \kinds^m$ that
  $\GEnv \vdash T[\mu  x. T/x] :: \kinds^m$.

  There are two interesting cases. In the first case, we encounter the
  recursion variable $\GEnv, x::\kinds^m, \GEnv' \vdash x
  :: \kinds^m$. At this point, we have to return $\GEnv, \GEnv' \vdash \mu
  x.T :: \kinds^m$, which is derivable by the initial assumption and
  weakening (Lemma~\ref{lemma:weakening-kind}).

  The other case is a different $\mu$ operator in a judgment $\GEnv,
  x::\kinds^m, \GEnv' \vdash \mu x'. T' :: \kinds^{m'}$. Inversion yields $\GEnv,
  x::\kinds^m, \GEnv', x'::\kinds^{m'} \vdash T'  :: \kinds^{m'}$ and $\GEnv,
  x:\gamma, \GEnv' \Contr T' : \gamma'$. The first part
  can be handled by induction, but the second part requires an
  auxiliary induction to prove that $(\GEnv, \GEnv')
  \Contr T'[\mu x.T/x] : \gamma'$. For this auxiliary induction it is
  sufficient to observe that a successful derivation never reaches a
  recursion variable, so the unrolling does not matter. 
\end{proof}

\begin{lemma}
  \label{lemma:app:unfold-fixpoints}
  If $T$ is a guarded type, then $\Unfold (T) = T$.
\end{lemma}
\begin{proof}
  Cases on $T$.

  \textbf{Case }$\skipk$: Obvious.

  \textbf{Case }$A$: $\Unfold (A) = A$.

  \textbf{Case }$(A; S')$:
  $\Unfold (A;S') = (A; S')$ as $\Unfold (A) = A$.

  \textbf{Case }$\star\{l_i\colon S_i\}$: Immediate.

  \textbf{Remaining cases}: Immediate.
\end{proof}

\begin{lemma}
  \label{lemma:app:unfold-idempotent}
  % For all well-formed types, $
  $\Unfold (\Unfold (T)) = \Unfold (T).$
\end{lemma}
\begin{proof}
  By Lemma~\ref{lem:unfold-yields-guarded-types}, $\Unfold (T)$ is guarded and hence a fixpoint of
  $\Unfold$ by Lemma~\ref{lemma:unfold-fixpoints}.
\end{proof}

\subsection{Type equivalence}
\label{sec:type-equivalence}

We want to define a notion of type equivalence for session types that
only depends on the communication behavior of a process with that
type. To this end, we first define a (weak) labelled transition system
$(\stypes, \Sigma, \LTSderives)$ that captures this behavior. The set
of states is  $\stypes = \{ S \mid \GEnv \vdash S :: \kinds^m \}$
where $\GEnv$ is an arbitrary, fixed kinding environment that binds no
recursion variables. The
actions in this system are drawn from the set $\Sigma = \{ {!B}, {?B}
\mid  B \in \btypes \} \uplus \Tyvars \uplus \{ {!l}, {?l} \mid l
\in \Labels\} $. We use the label $\Silent$ for the \emph{silent
  action} that exhibits no externally observable behavior. The transition
relation $\LTSderives$ is defined by the rule set in Figure~\ref{fig:type-behavior}. We write
$\Wderives$ for the reflexive transitive closure of $\LTSderives[\Silent]$ and $\Wderives[\xi]$ for
the composition $\Wderives \circ \LTSderives[\xi] \circ \Wderives$.

\begin{figure}[tp]
  \begin{gather*}
    {A \LTSderives[A] \skipk }
    \qquad
    {\star\{\overline{l_n:S_n}\} \LTSderives[\star l_i] S_i}
    \\
    \frac{S_1 \LTSderives[\xi] S_1'}{(S_1; S_2) \LTSderives[\xi]
      (S_1';S_2) }
    \qquad
    {(\skipk; S) \LTSderives[\Silent] S}
    \\
    {((S_1;S_2); S_3) \LTSderives[\Silent] (S_1; (S_2; S_3))}
    \\
    {(\star\{\overline{l_n:S_n}\}; S) \LTSderives[\Silent]
      \star\{\overline{l_n:(S_n; S)}\}}
    \\
    { \mu x.S \LTSderives[\Silent] S[\mu x.S/x]}
  \end{gather*}
  \caption{Behavior of a type}
  \label{fig:type-behavior}
\end{figure}
\begin{lemma}\label{lemma:app:unfold-silent}
  If $S' = \Unfold (S)$ is defined, then $S \Wderives S'$.
\end{lemma}
\begin{proof}
  By induction on the number of recursive calls to compute $\Unfold
  (S)$.

  \textbf{Case }$\Unfold (\mu x.S) = \Unfold (S[\mu x.S/x])$: By
  definition, $\mu x.S \LTSderives[\Silent] S[\mu x.S/x] $ and by
  induction $S[\mu x.S/x] \Wderives S'$.

  \textbf{Case }$\Unfold (S_1;S_2)$.

  \textbf{Subcase }$\Unfold (S_1) = \skipk$: By induction, $S_1
  \Wderives \skipk$. By the context rule for behaviors,
  $(S_1; S_2) \Wderives (\skipk; S_2 )$ and by the skip
  rule: $(\skipk; S_2 ) \LTSderives[\Silent] S_2$. Proceed by another
  induction on $S_2$.

  \textbf{Subcase }$\Unfold (S_1) = A$. By induction and the context
  rule.

  \textbf{Subcase }$\Unfold (S_1) = (S_1'; S_1'')$. By induction and
  the context rule, we obtain $(S_1;S_2) \Wderives
  ((S_1'; S_1''); S_2) \LTSderives[\Silent] (S_1'; (S_1''; S_2))$
  where the last step is an application of associativity.

  \textbf{Subcase }$\Unfold (S_1) = \star\{\overline{l_n:S_n} \}$. By
  induction and distributivity.

  \textbf{Remaining cases}. No silent transition needed.
\end{proof}

\begin{lemma}\label{lemma:app:silent-unfold-compatible}
  If $S\LTSderives[\Silent] S'$, then $\Unfold (S) = \Unfold (S')$.
\end{lemma}
\begin{proof}

  \textbf{Case }$\mu x.S \LTSderives[\Silent] S[\mu x.S/x]$:
  $\Unfold (\mu x.S) =  \Unfold (S[\mu x.S/x])$ by definition.

  \textbf{Case }$    {(\star\{\overline{l_n:S_n}\}; S) \LTSderives[\Silent]
    \star\{\overline{l_n:(S_n; S)}\}}$: Immediate by definition of
  $\Unfold$.
  
  \textbf{Case }${((S_1;S_2); S_3) \LTSderives[\Silent] (S_1; (S_2;
    S_3))}$: \\
  Case analysis on the possible outcomes of $\Unfold (S_1)$ and $\Unfold (S_2)$. 

  \textbf{Subcase } $\Unfold (S_1) = \skipk$:
  
  \textbf{Subsubcase }$\Unfold (S_2) = \skipk$:
  
  $\Unfold  (S_1; (S_2;  S_3)) = \Unfold (S_2; S_3) = \Unfold (S_3)$
  and
  $\Unfold ((S_1; S_2); S_3) = \Unfold (S_3)$.

  \textbf{Subsubcase }$\Unfold (S_2) = A$.

  $\Unfold  (S_1; (S_2;  S_3)) = \Unfold (S_2; S_3) = (A; S_3)$
  and
  $\Unfold ((S_1; S_2); S_3) = (A; S_3)$.

  \textbf{Subsubcase }$\Unfold (S_2) = (S_2'; S_2'')$.

  $\Unfold  (S_1; (S_2;  S_3)) = \Unfold (S_2; S_3) = (S_2' ;( S_2''; S_3))$
  and
  $\Unfold ((S_1; S_2); S_3) = (S_2'; (S_2''; S_3))$.

  \textbf{Subsubcase }$\Unfold (S_2) =
  \star\{\overline{l_n:S_n}\}$.

  $\Unfold  (S_1; (S_2;  S_3)) = \Unfold (S_2; S_3) =
  \star\{\overline{l_n:(S_n; S_3)} $
  and
  $\Unfold ((S_1; S_2); S_3) = \star\{\overline{l_n:(S_n; S_3)}$.

  \textbf{Subcase }$\Unfold (S_1) = A$.

  $\Unfold  (S_1; (S_2;  S_3)) =(A; (S_2; S_3))$
  and
  $\Unfold ((S_1; S_2); S_3) = (A; (S_2; S_3))$.

  \textbf{Subcase }$\Unfold (S_1) = (S_1';S_1'')$.

  $\Unfold  (S_1; (S_2;  S_3)) =(S_1'; (S_1''; (S_2; S_3)))$
  and
  $\Unfold ((S_1; S_2); S_3) = (S_1'; (S_1''; (S_2; S_3)))$.

  \textbf{Subcase }$\Unfold (S_1) = \star\{\overline{l_n:S_n}\}$.

  $\Unfold  (S_1; (S_2;  S_3)) =\star\{\overline{l_n:(S_n;
    (S_2\fatsemi S_3))}\} $
  and
  $\Unfold ((S_1; S_2); S_3) = \star\{\overline{l_n:(S_n; (S_2\fatsemi
    S_3)))}\}$.

  \textbf{Case }${(\skipk; S) \LTSderives[\Silent] S}$:
  $\Unfold (\skipk;S) = \Unfold (S) = S'$.

  \textbf{Case }$(S_1; S_2) \LTSderives[\Silent] (S_1';S_2)$
  because $S_1 \LTSderives[\Silent] S_1'$: By induction, $\Unfold
  (S_1) = \Unfold(S_1')$, so that $\Unfold (S_1; S_2) = \Unfold
  (S_1';S_2)$ by definition of unfolding.
\end{proof}
\begin{lemma}\label{lemma:app:silent-unfold}
  If $S \Wderives S'$ and $S'$ is guarded, then $S' = \Unfold (S)$.
\end{lemma}
\begin{proof}
  By induction on the number of silent steps.

  \textbf{Case }$0$. If $S=S'$ is already guarded, then $S$ is a
  fixpoint of $\Unfold$  by Lemma~\ref{lemma:unfold-fixpoints}.

  \textbf{Case }$n>0$. In this case, there is some $S''$ such that $S
  \LTSderives[\Silent] S''$ and $S'' \Wderives S'$ in less than $n$
  steps. Now, $S' = \Unfold (S'') = \Unfold (S)$, the former by
  induction  and the latter by
  Lemma~\ref{lemma:silent-unfold-compatible}.
\end{proof}

% A relation $R \subseteq \stypes \times \stypes$ is a \emph{type
%   simulation} if $(S_1,S_2)\in R$ implies the following conditions:
% %
% \begin{enumerate}
% \item If $\Unfold(S_1) = \skipk$ then $\Unfold(S_2) = \skipk$. 
% \item If $\Unfold(S_1) = (\alpha; S_1')$ then $\Unfold(S_2) =
%   (\alpha;S_2')$ and $(S_1', S_2') \in R$. 
% \item If $\Unfold(S_1) = (!B;S_1')$ then $\Unfold(S_2) = (!B;S_2')$  and $(S_1', S_2') \in R$. 
% \item If $\Unfold(S_1) = (?B;S_1')$ then $\Unfold(S_2) = (?B;S_2')$  and $(S_1', S_2') \in R$. 
% % \item If $\Unfold(S_1) = S_1';S_1''$ then $\Unfold(S_2) = S_2';S_2''$
% %   and both $(S_1',S_2')$  and $(S_1'',S_2'')$ are in $R$.
% \item If $\Unfold(S_1) = \oplus\{l_i\colon S_{1,i}'\}_{i\in I}$ then
%   $\Unfold(S_2) = \oplus\{l_i\colon S_{2,i}'\}_{i\in I}$ and
%   $(S_{1,i}',S_{2,i}')\in R$, for all $i\in I$.
% \item If $\Unfold(S_1) = \&\{l_i\colon S_{1,i}'\}_{i\in I}$ then
%   $\Unfold(S_2) = \&\{l_i\colon S_{2,i}'\}_{i\in I}$ and
%   $(S_{1,i}',S_{2,i}')\in R$, for all $i\in I$.
% \end{enumerate}

\begin{definition}
Define a monotone function~$F$ on $\stypes\times\stypes$ as 
follows. 
%
\begin{align*}
  F (R) &= \{ (S_1, S_2) \mid \Unfold (S_1) = \Unfold (S_2) = \skipk \}
  \\
        &\cup \{ (S_1, S_2) \mid \Unfold (S_1) = \Unfold (S_2) = A \}
  \\
        &\cup \{ (S_1, S_2) \mid
          \begin{array}[t]{@{}l}
            \Unfold (S_1) = (A; S_1'),\\
            \Unfold (S_2) = (A; S_2'), \\
            (S_1', S_2')  \in R \}
          \end{array}
  \\
        &\cup \{ (S_1, S_2) \mid
          \begin{array}[t]{@{}l}
            \Unfold (S_1) = \star\{l_i\colon S_{1,i}'\}_{i\in I}, \\
            \Unfold (S_2) = \star\{l_i\colon S_{2,i}'\}_{i\in I}, \\
            \forall i: (S'_{1,i}, S'_{2,i}) \in R \}
          \end{array}
  % \\
  %       &\cup \{ (S_1, S_2) \mid
  %         \begin{array}[t]{@{}l}
  %           \Unfold (S_1) = \oplus\{l_i\colon S_{1,i}'\}_{i\in I}, \\
  %           \Unfold (S_2) = \oplus\{l_i\colon S_{2,i}'\}_{i\in I}, \\
  %           \forall i: (S'_{1,i}, S'_{2,i}) \in R \}
  %         \end{array}
  % \\
  %       &\cup \{ (S_1, S_2) \mid
  %         \begin{array}[t]{@{}l}
  %           \Unfold (S_1) = \&\{l_i\colon S_{1,i}'\}_{i\in I}, \\
  %           \Unfold (S_2) = \&\{l_i\colon S_{2,i}'\}_{i\in I}, \\
  %           \forall i: (S'_{1,i}, S'_{2,i}) \in R \}
  %         \end{array}
\end{align*}

This function helps define (weak) bisimularity $\TypeEquiv$ for the labelled transition system
$(\stypes, \Sigma, \LTSderives)$ on well-formed session types as a greatest fixpoint: ${\TypeEquiv}
= \GFP (F)$. The 
definition relies on the $\Unfold$ function instead of using silent transitions which is sanctioned
by Lemmas~\ref{lemma:unfold-silent} and~\ref{lemma:silent-unfold}.
\end{definition}

Our goal is to use weak bisimilarity for type
equivalence. To this end, we need to establish that it is indeed an
equivalence relation.\footnote{This development can be extended to a
  relation on $\kindt$, but the extension is entirely standard.}
Reflexivity and symmetry follow directly from the definition. Transitivity requires some work.



\begin{lemma}
  Type  bisimilarity $\TypeEquiv$ is reflexive.
\end{lemma}
% \begin{proof}
%   Obvious.
% \end{proof}

\begin{lemma}
  Type bisimilarity $\TypeEquiv$ is symmetric.
\end{lemma}
% \begin{proof}
%   Obvious.
% \end{proof}

\begin{lemma}
  Type bisimilarity $\TypeEquiv$ is transitive.
\end{lemma}
\begin{proof}
  Let $R  = {\TypeEquiv} \circ {\TypeEquiv}$. Show that $R \subseteq F(R)$, which implies that $R
  \subseteq {\TypeEquiv}$. Observe that ${\TypeEquiv}
  \subseteq R$ because $\TypeEquiv$ is reflexive.

  Suppose that $S_1 \TypeEquiv S_2$ and $S_2 \TypeEquiv S_3$ so that $(S_1, S_3) \in R$.

  \textbf{Case }$\Unfold (S_1) = \skipk$ implies $\Unfold (S_2) =\skipk$, which in turn implies $\Unfold (S_3) =
  \skipk$. Hence, $(S_1, S_3) \in F (\emptyset)$.

  \textbf{Case }$\Unfold (S_1) = A$. Then it must be the case that
  $\Unfold (S_2) = A$ and also $\Unfold (S_3)=A$. Hence, $(S_1, S_3)
  \in F (\emptyset)$.

  \textbf{Case }$\Unfold (S_1) = (A; S_1')$ with $A$ either $\alpha$, $!B$, or $?B$. It must be the
  case that $\Unfold (S_2) = (A; S_2')$ with $(S_1', S_2') \in {\TypeEquiv}$ and further
  $\Unfold (S_3) = (A; S_3')$ with $(S_2', S_3') \in {\TypeEquiv}$. But then $(S_1', S_3') \in R =
  {\TypeEquiv} \circ {\TypeEquiv}$ and hence $(S_1, S_3) \in F (R)$.

  \textbf{Case }$\Unfold (S_1) = \star\{l_i\colon S_{1,i}'\}_{i\in I}$. Then it must be that
  $\Unfold (S_2) = \star\{l_i\colon S_{2,i}'\}_{i\in I}$ with $S_{1,i}' \TypeEquiv S_{2,i}'$, for all
  $i$, and $\Unfold (S_1) = \star\{l_i\colon S_{1,i}'\}_{i\in I}$ with  $S_{2,i}' \TypeEquiv S_{3,i}'$, for all
  $i$. Hence, $(S_{1,i}', S_{3,i}') \in R $, for all $i$, so that $(S_1, S_3) \in F (R)$.
  % \textbf{Case }$\Unfold (S_1) = \&\{l_i\colon S_{1,i}'\}_{i\in I}$. Analogously.
\end{proof}

\begin{lemma}
  \label{lem:app:unfold-type-sim}
  $S \TypeEquiv \Unfold(S)$.
\end{lemma}
\begin{proof}
  Straightforward application of coinduction. We show that $\{ (S, \Unfold (S)) \} \subseteq
  F (\TypeEquiv)$ because 
  of idempotence of $\Unfold$ (Lemma \ref{lemma:unfold-idempotent}) and reflexivity of $\TypeEquiv$.
\end{proof}

\begin{lemma}
  \label{lem:app:skip-elim}
  $\skipk;S_1 \TypeEquiv \skipk;S_2$ iff $S_1 \TypeEquiv S_2$.
\end{lemma}
\begin{proof}
  Immediate because $\Unfold (\skipk; S) = \Unfold (S)$.
\end{proof}

Outside this section, we write $\GEnv \vdash S_1 \TypeEquiv S_2$ to fix the environment $\GEnv$
needed for the formation of $S_1$ and $S_2$. As usual, $\GEnv$ must
not bind recursion variables.


\subsection{Translation to BPA}
\label{sec:translation-bpa}

We define a variant of the unfolding
function for a session type $S$,  $\Unravel(S)$, recursively by cases on the
structure of~$S$. The difference to $\Unfold (S)$ is that the
structure of $S$ is left intact as much as possible.
\begin{enumerate}
\item $\Unravel(\mu x.S) = \Unravel(S[\mu x.S/x])$
\item $\Unravel (S;S') = \left\{%
  \begin{array}{ll}
    \Unravel(S') & \Unravel(S) = \skipk
    \\
    (\Unravel(S); S') & \Unravel(S) \ne \skipk
  \end{array}
  \right.
$
\item $\Unravel (S) = S$ for all other cases
\end{enumerate}


To define the translation to BPA,
we need to show that, for a well-formed session type $S$, $\Unravel
(S)$ is always guarded. That is the output of $\Unravel (S)$ is either
$\skipk$ or it has one
of the following forms:
\begin{align*}
  O &::= A \mid \star\{\overline{l_i:S_i}\} \mid (O; S)
\end{align*}

Now we define the translation of well-formed $S$ to a BPA as
follows. Assume that all recursion variable bindings are unique in the
sense that the set $\{ \mu x_1.S_1, \dots, \mu x_n.S_n\}$ contains all $\mu$-subterms
of $S$ with $S_i : \Productive$ modulo $x_i \equiv \mu x_i.S_i$. 
Define the BPA process equations for $S$ by 
\begin{align*}
  \toBPATop{S} &= \{ \\
  x_0 &= \toBPA{S}, \\
               x_1&= \toBPA{\Unravel (S_1[\mu x_1.S_1/x_1])}, &
                                                                &\dots &
                                                     & \}
\end{align*}
\begin{align*}
  \toBPA{\skipk} &= \varepsilon \\
  \toBPA{A} &= A\\
  \toBPA{S_1;S_2} &= \toBPA{S_1} ; \toBPA{S_2} \\
  \toBPA{\star\{\overline{l_n:S_n}\}} &= (\star l_1;
                                        \toBPA{S_1} + \dots + \star
                                        l_n; \toBPA{S_n}) \\
  \toBPA{\mu x.S} &=
                    \begin{cases}
                      x & S : \Productive \\
                      \varepsilon & S : \Guarded
                    \end{cases}
                   \\
 \toBPA{x} &= x
\end{align*}
It is deliberate that we do \textbf{not} unfold the top-level type $S$
in the defining equation for $x_0$. This equation need not be guarded
because $x_0$ does not appear on the right-hand side of any equation.

Alternative: one could also define the equation extraction by
induction on the kind derivation for $S$ and the right-hand side
extraction by induction on the contractivity judgment. 

\begin{lemma}
  If $\mu x.S$ is a subterm of well-formed $S_0$ with $\Delta \Contr S:\Productive$, then
  $\toBPA{\Unravel (S)}$ is guarded with respect to $\toBPATop{S_0}$. 
\end{lemma}

\begin{proof}
  By Lemma~\ref{lem:unfold-yields-guarded-types}, we know that
  $\Unravel (S)$ yields a term of the form $A$, $A;S'$ or
  $\star\{\overline{l_n:S_n}\}$. Clearly, the translation of a term of
  one of these forms is guarded.

  If $\mu x.S$ has a free variable $x' \ne x$, then
  $\toBPA{\Unravel (S)}$ may have the form $x'$ or $x';S'$.
\end{proof}

It remains to show that $S$ is bisimilar to its
translation. Essentially, we want to prove that the function
$\toBPA{\cdot}$ is a bisimilation when considered as a relation.

\begin{lemma}\label{lemma:app:skip-implies-done}
  If $\Unravel (S) = \skipk$, then $\DONE{S}$.
\end{lemma}
\begin{proof}
  Induction on the number $n$ of recursive calls to $\Unravel$.

  \textbf{Case }$n=0$. $S=\skipk$ and $\DONE{\skipk}$.

  \textbf{Case }$n>0$.

  \textbf{Subcase }$\mu x.S$. $\Unravel (\mu x.S) = \skipk$ 
  because $\Unravel (S[\mu x.S/x]) = \skipk$. By induction,
  $\DONE{S[\mu x.S/x]}$ and by applying the mu-DONE rule $\DONE{\mu
    x.S}$.

  \textbf{Subcase }$S_1;S_2$.
  $\Unravel (S_1;S_2) = \skipk$ because $\Unravel (S_1) =
  \Unravel (S_2) = \skipk$. By induction $\DONE{S_1}$ and
  $\DONE{S_2}$. By rule seq-DONE $\DONE{S_1;S_2}$.
\end{proof}

\begin{lemma}\label{lemma:app:s=unr-s}
  Let $S$ be closed, well-formed. \\
  Then
  $\toBPA{S} \TypeEquiv \toBPA{\Unravel (S)}$.
\end{lemma}
\begin{proof}
  Induction on the number $n$ of recursive calls to $\Unravel$.

  \textbf{Case }$n=0$. In this case, $S$ must be $\skipk$, $A$, or
  $\star\{\overline{l_i:S_i}\}$ and the claim is immediate.

  \textbf{Case }$n>0$. There are two subcases.

  \textbf{Subcase }$\mu x.S$. Then $\toBPA{\mu x.S} = x$ and there is
  an equation $x = \toBPA{\Unravel(S[\mu x.S])}$. Now, $x$ is
  obviously bisimilar to $\toBPA{\Unravel(S[\mu x.S])}$.

  \textbf{Subcase }$S_1;S_2$. If $\Unravel (S_1) = \skipk$, then
  $\DONE{S_1}$ and hence $\DONE{\toBPA{S_1}}$. Furthermore, $\Unravel
  (S_1;S_2) = \Unravel (S_2)$ and, by induction, $\toBPA{S_2}
  \TypeEquiv \toBPA{\Unravel(S_2)}$. The result follows because
  $\toBPA{S_1;S_2} = \toBPA{S_1};\toBPA{S_2} \TypeEquiv \toBPA{S_2}$
  and $\toBPA{\Unravel(S_2)} = \toBPA{\Unravel (S_1;S_2)}$.

  If $\Unravel (S_1) =: S_u \ne \skipk$, then $\Unravel (S_1;S_2) =
  S_u; S_2$.
  By induction, we know that $\toBPA{S_1} \TypeEquiv \toBPA{S_u}$ and
  as bisimilarity is a congruence  $\toBPA{S_1;S_2} \TypeEquiv
  \toBPA{S_u}; \toBPA{S_2} = \toBPA{\Unravel (S_1;S_2)}$.
\end{proof}

\begin{lemma}\label{lemma:app:bisimulation-BPA-forwards}
  Suppose $S$ is a well-formed closed session type.
  If $S \LTSderives S'$, then $\toBPATop{S}
  \LTSderives \toBPATop{S'}$.
\end{lemma}
\begin{proof}
  By induction on  $S \LTSderives S'$.

  \textbf{Case }$A \LTSderives[A] \skipk$.
  In this case $\toBPATop{A} = \{ x_0 = A \} \LTSderives[A] \{ x_0 =
  \varepsilon \} = \toBPATop{\skipk}$.

  \textbf{Case }$\star\{\overline{l_i:S_i}\} \LTSderives[\star l_i]
  S_i$.
  In this case $\toBPATop{\star\{\overline{l_i:S_i}\}} = \{ x_0 =
  (\dots+ \star l_i; \toBPA{S_i} + \dots) \} \LTSderives [\star l_i]
  \{ x_0 = \toBPA{S_i} \} =  \toBPATop{S_i}$.

  \textbf{Case }$\frac{S_1 \LTSderives S_1'}{S_1; S_2
    \LTSderives S_1';S_2}$.
  In this case $\toBPATop{S_1;S_2} = \{ x_0 = E_1;E_2, \dots \}$ where
  $E_i = \toBPA{S_i}$ for $i=1,2$.
  Because $S_1 \LTSderives S_1'$,
  we obtain by induction that $\toBPATop{S_1} = \{ x_0 = E_1, \dots \}
  \LTSderives \toBPATop{S_1'} = \{ x_0 = E_1', \dots
  \}$. Therefore, $\{ x_0 = E_1;E_2, \dots \} \LTSderives \{ x_0 =
  E_1';E_2, \dots \} = \toBPATop{S_1'; S_2}$.

  \textbf{Case }$\frac{\DONE{S_1} \quad S_2 \LTSderives S_2'}{S_1; S_2
    \LTSderives S_2'}$.
  In this case $\toBPATop{S_1;S_2} = \{ x_0 = E_1;E_2, \dots \}$ where
  $E_i = \toBPA{S_i}$ for $i=1,2$.
  It is easy to see that $\DONE{S_1}$ implies $\DONE{\toBPA{S_1}}$,
  that is, $\DONE{E_1}$.
  Because $S_2 \LTSderives S_2'$,
  we obtain by induction that $\toBPATop{S_2} = \{ x_0 = E_2, \dots \}
  \LTSderives \toBPATop{S_2'} = \{ x_0 = E_2', \dots
  \}$. 
  Therefore, $\{ x_0 = E_1;E_2, \dots \} \LTSderives \{ x_0 =
  E_2', \dots \} = \toBPATop{S_2'}$.

  \textbf{Case }$\frac{S[\mu x.S/x] \LTSderives S'}{\mu x.S
    \LTSderives S'}$.
  In this case
  $\toBPATop{\mu x.S} = \{ x_0 = x, x = E, \dots \}$ with $E = \toBPA{\Unravel
    (S[\mu x.S/x])}$.
  By induction, $\toBPATop{S[\mu x.S/x]} \LTSderives
  \toBPATop{S'}$.
  Now $\toBPATop{S[\mu x.S/x]} = \{ x_0 = \toBPA{S[\mu x.S/x]}, \dots
  \}$ which proves the claim because $x_0 \TypeEquiv E$ by
  Lemma~\ref{lemma:s=unr-s}. 
\end{proof}

\clearpage
\begin{lemma}\label{lemma:app:bpa-unr-s}
  Suppose that $\toBPA{\Unravel
    (S)} \LTSderives E'$. Then $S \LTSderives S'$
  and $E' = \toBPATop{S'}$.
\end{lemma}
\begin{proof}
  By induction on the number $n$ of recursive calls of $\Unravel$.

  \textbf{Case }$n=0$.

  \textbf{Subcase }$S=\skipk$. Contradictory.

  \textbf{Subcase }$S=A$. Then $a=A$, $E'=\varepsilon$, and $S' =
  \skipk$.

  \textbf{Subcase }$S = \star\{\overline{l_i:S_i}\}$. Then $a = \star
  l_i$, $E' = \toBPA{S_i}$, and $S' = S_i$.

  \textbf{Case }$n>0$.

  \textbf{Subcase }$S = S_1;S_2$.
  If $\Unravel (S_1) = \skipk$, then $\Unravel (S) = \Unravel
  (S_2)$ with less than $n$ calls. As $\toBPA{\Unravel (S_2)} \LTSderives E'$, induction yields
  that $S_2 \LTSderives S'$ and $E' = \toBPATop{S'}$.
  As $\Unravel (S_1) = \skipk$, we know that $\DONE{S_1}$. Hence,
  ${S_1;S_2} \LTSderives S'$ and $E' = \toBPATop{S'}$.

  If $\Unravel (S_1)\ne \skipk$, then consider $\toBPA{\Unravel({S_1});
    S_2} \LTSderives E'$ because $\toBPA{\Unravel({S_1})} \LTSderives
  E_1'$, so that induction yields some $S_1'$ such that $S_1
  \LTSderives S_1'$ and $E_1' = \toBPATop{S_1'}$.

  \textbf{Subcase }$\mu x.S$.
  $\Unravel (\mu x.S) = \Unravel (S[\mu x.S/x])$ with one less
  invocation. As $\toBPA{\Unravel (S[\mu x.S/x])}
  \LTSderives E'$, induction yields that  $S[\mu x.S/x] \LTSderives
  S'$ with $E' = \toBPATop{S'} $.
\end{proof}

\begin{lemma}\label{lemma:app:bisimulation-BPA-backwards}
  Suppose that $S$ is well-formed and let $\BPAprocess = \toBPATop{S}$
  and $\BPAprocess \LTSderives \BPAprocess'$.

  There is some $S'$ such that $S \LTSderives S'$ and $\BPAprocess' = \toBPATop{S'}$.
\end{lemma}
\begin{proof}
  By induction on $S$.

  \textbf{Case }$\skipk$. Contradictory.

  \textbf{Case }$A$. For $\BPAprocess$, ${A \LTSderives \varepsilon
  }$. Choose $S' = \skipk$.

  \textbf{Case }$\star\{\overline{l_i:S_i}\}$. For $\BPAprocess$,
  ${\sum \overline{\star l_i; \toBPA{S_i}} \LTSderives[\star l_i]
    \toBPA{S_i} }$. Choose $S' = S_i$.

  \textbf{Case }$S_1;S_2$.
  If $\toBPA{S_1} \LTSderives E_1'$,
  then $S_1 \LTSderives S_1'$ and $E_1' = \toBPA{S_1'}$, by induction.
  Now, $\toBPA{S_1;S_2} \LTSderives E_1'; \toBPA{S_2} = \toBPA{S_1';
    S_2}$. Choose $S' = S_1';S_2$.

  If $\DONE{\toBPA{S_1}}$ and $\toBPA{S_2} \LTSderives E_2'$,
  then $\DONE{S_1}$ and $S_2 \LTSderives S_2'$ and $E_2' =
  \toBPA{S_2'}$, by induction. Now, $\toBPA{S_1;S_2} \LTSderives E_2' = \toBPA{S_2'}$. Choose $S' = S_2'$.

  \textbf{Case }$\mu x.S$.
  $\BPAprocess = \toBPATop{\mu x.S} = \{ x_0 = x, x = \toBPA{\Unravel
    (S[\mu x. S/x])} \}$. If $\BPAprocess \LTSderives \BPAprocess'$,
  then it must be because $\toBPA{\Unravel
    (S[\mu x. S/x])} \LTSderives E'$. Use Lemma~\ref{lemma:bpa-unr-s}
  to establish the claim.
\end{proof}

\begin{theorem}
  Suppose that $S$ is well-formed and let $\BPAprocess = \toBPATop{S}$.
  \begin{enumerate}
  \item If $ S \LTSderives[a] S'$, then $\BPAprocess \BPAderives[a]
    \BPAprocess'$ with $\BPAprocess' = \toBPATop{S'}$.
  \item If $\BPAprocess \BPAderives[a] \BPAprocess'$, then $S
    \LTSderives[a] S'$ with  $\BPAprocess' = \toBPATop{S'}$.
  \end{enumerate}
\end{theorem}
\begin{proof}
  By Lemmas~\ref{lemma:bisimulation-BPA-forwards} and~\ref{lemma:bisimulation-BPA-backwards}.
\end{proof}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
               doc/ICFP2016/popl16-response.txt                                                                    0000664 0001750 0001750 00000010657 13216017506 016465  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               Thanks to the reviewers for their comments.

Reviewer#A: first order session types

The goal here is to provide a typed ground for low-level serialization, to be used in distributed systems where channel passing is difficult, if not impossible, to accomplish.

No doubt that the code in SILL would solve the problem, but at the expense of channel creation and passing, something that we want to avoid. A similar exercise could be conducted on the Gay and Vasconcelos language [8]. (we'll add the reference to SILL, of course)
Needless to say that the repeated channel creation is costly and counter to our intention to have a low-level system
that governs a protocol on a single channel.

We tried the second alternative (where the session type over-approximates the legal communications) using the expression server (section 2.4). For that we used the pi calculus with session types (more concretely, the SePi language, SEFM'14, LNCS 8368, another missed reference), and ended up with 100+ lines of code (excluding a stack encoding) that incorporate all sorts of runtime checks. This certainly not the way to go.

We will certainly have a look at the third alternative (dependently typed session types), but the immediate benefit is not obvious, as stated by the reviewer. A preliminary investigation shows that the sendtree operation should be easy to
type using dependent types; something like
(t : Tree) -> Channel t
- where Channel is a suitable type level function - will do.
However, it may be tricky to write the receiving end. Essentially, you get a channel of type
exists t. Channel t
but this means you'd first have to have t before you can elaborate the type of the channel to receive t.
That seems impossible to accomplish.

Reviewer#B: polymorphic continuation channels

Exactly. The act of sending a message is unrelated to whatever comes next on the channel.

Reviewer#B: servers that avoid unnecessary communication

We are not arguing for protocols that depend on the values transmitted; for that we would need dependent types, as suggested by the reviewer. Instead we let the server decide whether it requires further subterms, and communicate the decision to the client, as captured with a type such as:

type TermChan = +{
  Const : !int,
  Add : TermChan ; TermChan,
  Mult : TermChan ; &{more: TermChan, done: skip}
}

We don't understand the comment negating "lack of space". The CFP says: "Each paper should have no more than 12 pages of text, excluding bibliography, in at least 9 pt format." and we are at 12 pages + bibliography.

Reviewer#B:  synchronous and buffered semantics

Synchronous semantics is rendez-vous based: blocking output and blocking input. Buffered semantics assumes underlying unbounded buffers from where processes may read and write. As a result we have unblocking output (and possibly blocking input).

Reviewer#C: larger class than context-free?

Theorem 3.18 shows that our syntax yields omega-context-free traces.

Reviewer#C: bisimulation

The underlying graph is the obvious transition system where the nodes are session types and transitions are labeled with the trace action (Def.3.2). The bisimulation acts on that graph as in the related paper by Gay and Hole.

Reviewer#C: Lemma 3.12: straightforward application of coinduction: why?

\approx is defined as a gfp and the proof applies Tarski's fixpoint theorem.

Reviewer#C: "separate coinductive argument"

It's a routine application of Tarski.

Reviewer#C: weak by inspection-shaky proof

By examining the construction, it is clear that all DPDAs have the same number of states as the DPDA in Fig.3. Hence, they have the same structure. Fig.3 defines the priorities in line 1 and it is trivial to see that each transition increases the priority.

Reviewer#C suggests applying Bonchi&Pous

The speciality of Bonchi&Pous is the definition of a clever bisimulation-up-to. Moreover, their work applies to regular languages, whereas we are concerned with omega-context-free languages. We don't see how their method could be applicable to our problem as we are dealing with DPDAs, not NFAs.

Reviewer#D: is the result on the decidability of type equivalence significant enough

It implies that type checking is decidable.

Reviewer#D: conjecture (3.16) and its relation to Theorem 3.20

The proof wasn't finished by the time of the submission, but it turned out to be straightforward.
Conjecture 3.16 is needed in the proof of 3.20:
One direction of the "iff" in the first line of the proof is 3.16, the other is 3.17.
                                                                                 doc/ICFP2016/Makefile                                                                               0000664 0001750 0001750 00000000376 13216017506 014404  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               paper48.zip: main.tex biblio.bib main.bbl macros.tex intro.tex examples.tex types.tex processes.tex related.tex conclusion.tex
	zip paper48 main.tex biblio.bib main.bbl macros.tex intro.tex examples.tex types.tex processes.tex related.tex conclusion.tex
                                                                                                                                                                                                                                                                  doc/ICFP2016/tree.cfs                                                                               0000664 0001750 0001750 00000001466 13216017506 014401  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               type XformChan = oplus{
  Leaf: skip,
  Node: !int;XformChan;XformChan;?int}

transform : forallalpha.Tree -> XformChan;alpha -> Tree otimes alpha
transform Leaf c =
  (Leaf,select Leaf c)
transform (Node x l r) c =
  let c1 = select Node c
      c2 = send c x
      l1,c3 = transform l c2
      r1,c4 = transform r c3
      x1,c5 = receive c4
  in (Node x1 l1 r1,c5)
  
treeSum : forallalpha.dualof XformChan; alpha -o int otimes alpha
treeSum c =
  case c of
    Leaf: lambdac1.(0,c1)
    Node: lambdac1.let x,c2 = receive c1
                  l,c3 = treeSum c2
                  r,c4 = treeSum c3
                  c5 = send c4 (x+l+r)
              in (x+l+r,c5)

aTree = Node 3 Leaf (Node 4 Leaf Leaf)

go : Tree -> Tree
go aTree =
  let c, s = new XformChan
  in fork (fst (treeSum s));
     fst (transform aTree c)
                                                                                                                                                                                                          doc/ICFP2016/arithmetic-server.cfs                                                                  0000664 0001750 0001750 00000001633 13216017506 017073  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               type TermChan = oplus{Const: !int,
	          Add:TermChan;TermChan,
	          Mult: TermChan;TermChan}

computeService : dualof TermChan;!int -> skip
computeService c =
  let n1,c1 = receiveEval c
  in send n1 c1

receiveEval : forallalpha.dualof TermChan;alpha -> int otimes alpha
receiveEval c =
  case c of {
    Const: lambdac.receive c
    Add:  lambdac.let n1,c1 = receiveEval c
                 n2,c2 = receiveEval c1
	     in (n1+n2,c2)
    Mult: lambdac.let n1,c1 = receiveEval c
                 n2,c2 = receiveEval c1
	     in (n1*n2,c2)
  }

client : TermChan;?int -> int otimes skip
client c =
  let c1 = select Add c
      c2 = select Const c1
      c3 = send 5 c2
      c4 = select Mult c3
      c5 = select Const c4
      c6 = send 7 c5
      c7 = select Const c6
      c8 = send 9 c7
    in receive c8
      
go : int
go =
  let c, s = new TermChan;?int
  in fork (computeService s);
     fst (client c)
                                                                                                     doc/ICFP2016/results.tex                                                                            0000664 0001750 0001750 00000024365 13216017506 015173  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               %\subsection{Soundness}

% \begin{lemma}[Strengthening]
%   \label{lem:strengthening}
%   If $\Delta;\Gamma,a\colon T \vdash p$ and~$a$ not free in~$p$ then
%   $\Delta;\Gamma \vdash p$ and~$\un(T)$.
% \end{lemma}
% %
% \begin{proof}
%   By rule induction on the first hypothesis.
% \end{proof}

% \begin{lemma}[Congruence]
%   \label{lem:congruence}
%   If $\Gamma \vdash p$ and $p\equiv q$ then $\Gamma \vdash q$.
% \end{lemma}
% %
% \begin{proof}
%   By rule induction on the first hypothesis, using strengthening
%   (Lemma~\ref{lem:strengthening}).
% \end{proof}

% \begin{lemma}[Substitution]
%   \label{lem:subs}
%   If $\Delta;\Gamma_1, a\colon T_2 \vdash e_1 : T_1$ and
%   $\Delta;\Gamma_2 \vdash e_2 : T_2$ then
%   $\Delta;\Gamma_1,\Gamma_2 \vdash e_1 \subs{e_2}{a} : T_1$.
% \end{lemma}
% %
% \begin{proof}
%   By rule induction on the first hypothesis.
% \end{proof}

% \begin{lemma}[Sub-derivation introduction]
%   \label{lem:derivation-intro}
%   If $\mathcal D_1$ is a derivation of
%   $\Delta;\Gamma \vdash E[e] : T_1$ then $\Gamma = \Gamma_1,\Gamma_2$
%   and $\mathcal D_1$ has a sub-derivation $\mathcal D_2$ concluding
%   $\Delta;\Gamma_2 \vdash e : T_2$ such that the position of
%   $\mathcal D_2$ in $\mathcal D_1$ corresponds to the hole in $E[]$.
% \end{lemma}

% \begin{lemma}[Sub-derivation  elimination]
%   \label{lem:derivation-elim}
%   If $\mathcal D_1$ is a derivation of
%   $\Delta;\Gamma_1,\Gamma_2 \vdash E[e_1] : T_1$ and $\mathcal D_2$ is a
%   sub-derivation of $\mathcal D_1$ concluding
%   $\Delta;\Gamma_2,\Gamma_3 \vdash e_1 : T_2$ and the position of
%   $\mathcal D_2$ in $\mathcal D_1$ corresponds to the hole in $E[]$
%   and $\Delta;\Gamma_2 \vdash e_2 : T_2$ then
%   $\Delta;\Gamma_1,\Gamma_2,\Gamma_2 \vdash E[e_2] : T_1$.
% \end{lemma}

% The structural type rules (weak, copy, and $\equiv$) commute. The
% following fat rule is admissible.
% %
% \begin{equation*}
%   \frac{
%     \Delta;\Gamma_1, \Gamma_2, \Gamma_2 \vdash e : T_1
%     \quad
%     \un(\Gamma_2,\Gamma_3)
%     \quad
%     \Delta \vdash T_1 \TypeEquiv T_2
%   }{
%     \Delta;\Gamma_1, \Gamma_2, \Gamma_3 \vdash e : T_2
%   }
% \end{equation*}
% %
% Notice that if we replace, in the weak rule, the proviso
% $a\notin\Gamma$ by $a:U\in\Gamma \Rightarrow U\equiv T$, then copy
% and weak do not commute anymore.  This fat rule forms the basis for
% inversion below.

% \begin{lemma}[Inversion]\
%   \label{lem:inversion}
%   \begin{itemize}
%   \item If $\Gamma \vdash e$ then
%     $\Gamma = \Gamma_1, \Gamma_2, \Gamma_3$ and
%     $\un(\Gamma_2,\Gamma_3)$ and
%     $\Gamma_1,\Gamma_2,\Gamma_2 \vdash e\colon \sigma$ and
%     $\un(\sigma)$.
%   % \item If $\Gamma \vdash \letk\,ab=(u,v)\,\ink\,e : \sigma$ then
%   %   $\Gamma = \Gamma_1, \Gamma_2, \Gamma_3$ and
%   %   $\Gamma_1,\Gamma_2,\Gamma_2 = \Gamma_4,\Gamma_5,\Gamma_6,\Gamma_7$
%   %   and $\Gamma_5,\Gamma_6,\Gamma_6 = \Gamma_7,\Gamma_8$ and
%   %   $\Gamma_4,a\colon T_1, b\colon T_2 \vdash e \colon \sigma_1 \equiv
%   %   \sigma$
%   %   and $\Gamma_7 \vdash u: \sigma_2 = T_1$ and
%   %   $\Gamma_8 \vdash : \sigma_3 = T_2$.
%   \item %let/2
%     If $\Gamma \vdash \letin{ab}{e_1}{e_2} : \sigma$ then
%     $\Gamma = \Gamma_1, \Gamma_2, \Gamma_3$ and
%     $\un(\Gamma_2,\Gamma_3)$ and
%     $\Gamma_4,\Gamma_5 = \Gamma_1, \Gamma_2,\Gamma_2$ and
%     $\Gamma_4 \vdash e_1: T_1 \otimes T_2$ and
%     $\Gamma_5,a\colon T_1,b\colon T_2 \vdash e_2: \tau = \sigma$.
%   \item % (e1,e2)
%     If $\Gamma \vdash (e_1, e_2) : T$ then
%     $\Gamma = \Gamma_1, \Gamma_2, \Gamma_3$ and
%     $\un(\Gamma_2,\Gamma_3)$ and
%     $\Gamma_4,\Gamma_5 = \Gamma_1, \Gamma_2,\Gamma_2$ and
%     $\Gamma_4 \vdash e_1: T_2$ and $\Gamma_5 \vdash e_2:T_2$ and
%     $T_1\otimes T_2 = \sigma$
%   \item If $\Gamma \vdash \fork\,e:\sigma$ then
%     $\Gamma = \Gamma_1,\Gamma_2,\Gamma_3$ and
%     $\Gamma_1,\Gamma_2,\Gamma_2 \vdash e : \unitk$ and $\un(\Gamma$)
%     and $\sigma \equiv \unitk$.
%   \item If $\Gamma \vdash \newk : \sigma$ then $\un(\Gamma)$ and
%     $\sigma \equiv S\otimes\dual S$.
%   \item If $\Gamma_1 \vdash \recvk\, a : \sigma_1$ then
%     $\sigma_1 \equiv T \otimes S$ and 
%     $\Gamma_1 = \Gamma_2, a\colon \sigma_2$ and $a\notin\Gamma_2$ and
%     $\sigma_2 \equiv \;?B;S$.
%   \item If $\Gamma \vdash \sendk\, e\, a : \sigma$ then
%     $\sigma \equiv S$ and
%     $\Gamma = \Gamma_1, \Gamma_2, \Gamma_3, a\colon \sigma_2$ and
%     $\un(\Gamma_2,\Gamma_3)$ and $\sigma_2 \equiv \;!B;S$ and
%     $\Gamma_1,\Gamma_2, \Gamma_2 \vdash e \colon \sigma_1$ and
%     $\sigma_1\equiv T$.
%   \end{itemize}
  
% \end{lemma}

% \begin{theorem}[Soundness]
%   \label{thm:soundness}
%   If $\Gamma \vdash p$ and $p \rightarrow q$ then $\Gamma \vdash q$.
% \end{theorem}
% %
% \begin{proof}
%   By rule induction on the second premise.

%   Case the derivation ends with the rule for communication: inversion
%   ($\newk$, par, proc twice), sub-derivation intro (twice), inversion
%   ($\sendk$, $\recvk$), rules var and $\equiv$, sub-derivation elim
%   (twice), rules proc (twice) and par and weak and copy, definition of
%   $\dual S$, rule $\newk$.

%   Case the derivation ends with the rule for branching: should be
%   similar to the above, but simpler (did not check).

%   Case the derivation ends with channel creation: inversion,
%   sub-derivation intro, inversion, var axiom, $\otimes$ intro,
%   sub-derivation elimination, rules proc and $\newk$.

%   Case the derivation ends with $\forkk$: sub-derivation intro,
%   inversion, copy, sub-derivation elimination, rules proc and par and
%   copy.

%   Case the derivation ends with par: inversion, induction, rule $\mid$.

%   Case the derivation ends with reduction under $\newk$: inversion,
%   induction, rule $\newk$.

%   Case the derivation ends with $\equiv$: congruence lemma, induction.

%   Case the derivation ends with context: derivation lemma, induction,
%   derivation lemma.

%   Case the derivation ends with $\beta$: inversion, substitution
%   lemma.

%   Case the derivation ends with $\letk$: rule proc, inversion,
%   substitution lemma (twice), rules weak and copy.

%   Case the derivation ends with $\matchk$: inversion, application
%   rule.

%   Case the derivation ends with $\fixk$: inversion, substitution
%   lemma, contraction (copy rule).
% \end{proof}

% \begin{theorem}[Type safety]
%   If $\Delta;\Gamma \vdash p$ then $p$ is not an error.
% \end{theorem}
% %
% \begin{proof}
%   A simple analysis of the typing derivation for the hypothesis. We
%   analyse one such case.
% %
%   If $(\new ab)(E_1[e_1] \mid E_2[e_2] \mid p)$ and $\subj(e_1)=a$ and
%   $\subj(e_2)=b$ where $E_1$ does not bind~$a$ and $E_2$ does not
%   bind~$b$, then it must be the case that $\agree^{ab}(e_1,e_2)$.
%   % 
%   In fact, the structural typing rules and those for $\newk$ and for
%   parallel composition guarantee that
%   $\Delta;\Gamma_1,a\colon S \vdash E_1[e_1]$ and
%   $\Delta;\Gamma_2,b\colon \dual S \vdash E_2[e_2]$, for some
%   $\Delta,\Gamma_1,\Gamma_2$. When $e_1$ is $\send{e'_1}a$,
%   sub-derivation introduction and inversion
%   (lemmas~\ref{lem:derivation-intro} and~\ref{lem:inversion}) allow to
%   conclude that $\Delta \vdash S \TypeEquiv \;!B.S'$,
%   hence~$\Delta \vdash \dual S \TypeEquiv \;?B.\dual{S'}$. Of all the
%   cases terms with subject~$b$ only $\recv b$ has a type of the form
%   $?B.\dual{S'}$, hence $\agree^{ab}(\send{e'_1}a,\recv b)$.
% \end{proof}

% \section{Conservative Extension}
% \label{sec:conservative-extension}

% Our system is a conservative extension of previous session type
% systems. In those systems, the session type language is restricted to
% tail recursion, the $\mu$ operator works with a much simpler notion
% of contractivity, and equivalence is defined modulo unfolding.
% We take the definitions from the functional session type
% calculus~\cite{DBLP:journals/jfp/GayV10} as a blueprint.
% The first-order part of the session type language from that paper may
% be defined by $S'$ in the following grammar. Henceforth, we call that
% language \emph{regular session types}.
% \begin{align*}
%   S'_X & \grmeq \End \grmor !B.S''_X \grmor ?B.S''_X \\
%        &\grmor \oplus\{l_i\colon {S_i}_X''\} \grmor
%          \&\{l_i\colon {S_i}_X''\}
%   \\
%   & \grmor \mu x. S'_{X\cup\{x\}} \\
%   S''_X & \grmeq x\in X \grmor S'_X
% \end{align*}
% The translation $\Embed{}$ into our system is defined as follows.
% \begin{align*}
%   \Embed{\End} & = \skipk \\
%   \Embed{!B.S''} & = (!B; \Embed{S''}) \\
%   \Embed{?B.S''} & = (?B; \Embed{S''}) \\
%   \Embed{\oplus\{l_i\colon S_i''\}} & = \oplus\{l_i\colon \Embed{S_i''}\} \\
%   \Embed{\&\{l_i\colon S_i''\}} & = \&\{l_i\colon  \Embed{S_i''}\} \\
%   \Embed{\mu x.S'} & = \mu x. \Embed{S'} \\
%   \Embed{x} & = x
% \end{align*}
% \begin{lemma}
%   For all $S'_\emptyset$, $\cdot \vdash \Embed{S'_\emptyset} :: \kinds^\Linear$.
% \end{lemma}
% \begin{proof}
%   We need to prove a more general property.
%   Define $\GEnv_X = x : \kinds^\Linear \mid x \in X$.
%   For all $S'_X$, $\GEnv_X \vdash \Embed{S'_X} :: \kinds^\Linear$. The
%   proof is by straightforward induction.
% \end{proof}
% \begin{lemma}
%   Let $\vdash_{\text{GV}}$ be the typing judgment for expressions from Gay and
%   Vasconcelos~\cite{DBLP:journals/jfp/GayV10}.
%   If $\Gamma \vdash_{\text{GV}} e : T$, then $\cdot, \Gamma \vdash e : \Embed{T}$.
% \end{lemma}

% \begin{proposition}
%   The trace language of a regular session type is an $\omega$-regular language.
% \end{proposition}
% \begin{proof}
%   To see this result, we consult the translation $\toCFG$ from session types to
%   context-free grammars from Theorem~\ref{theorem:tr-is-w-CFL}. It is
%   easy to see that the image of $\toCFG$
%   after eliminating chain productions is a right-linear grammar,
%   when restricted to the image of the embedding of regular session
%   types $\Embed{\_}$.

%   Examination of the transformation of the grammar to a DPDA in
%   Proposition~\ref{prop:trace-is-weak-wpda} shows that every run of the automaton resulting from
%   transforming a right-linear grammar has at most one symbol on its stack. Hence, the stack can
%   be eliminated, which transforms the DPDA to a finite state $\omega$-automaton. This class of
%   automata recognizes exactly the class of $\omega$-regular languages \cite{DBLP:books/el/leeuwen90/Thomas90}.
% \end{proof}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                           doc/ICFP2016/icfp16-responses.txt                                                                   0000664 0001750 0001750 00000010032 13216017506 016602  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               Thanks to the reviewers for their thoughtful comments.

** decidability of type equivalence vs undecidability of CFG equivalence

Type equivalence is proved by reduction to BPA equivalence. 
It is also related to equivalence of *deterministic*  context-free languages,
which is also decidable.

It was surprising (to the authors) that type equivalence is decidable without
any restrictions. We were expecting to impose restrictions to obtain decidability.

** -> vs -o

Our apologies for the confusion. 
Our setup (which is consistent) is influenced by the cited work of Gay and Vasconcelos, 
which seems to use the lollipop symbol in a non-standard way.
Each type has a linearity attribute formalized with predicate un(T):
* T1 -> T2  is the type of a function that may be used multiple times
It can subsume to
* T1 -o T2 is the type of a function that must be used exactly once
* sessions must be used exactly once
* T1  (x) T2 is the type of a tensor that must be used exactly once

[standard usage of the lollipop indicates a function that uses its argument linearly]

The "possible counterexample" (Review A), \x.(x,x), does not allow to
copy a linear parameter: in the derivation of

     |- \x.(x,x) : T -> (T tensor T)

there is one undischarged assumption, un(T), coming from the copy rule.

** lemma 3.3

The bisimulation needed for this proof is 
R={(S1,S2) | S1 terminating and S2 terminating}.
It's a bisimulation because a terminating type
cannot make a step.

** proofs in 4.4

The proofs are similar to those in reference [11], section 6. 
The main result (Thm 4.9, Soundness) relies on exactly the same
lemmata (congruence and the substitution lemmas, sub-derivation
introduction and elimination, and inversion). Linearity crucially
comes into play in the substitution lemma where the assumption for the
replaced variable is removed from the typing context.

** corollary 4.11 and progress for the full language

It should be easy to see that the full language does not enjoy
progress. Consider two processes exchanging messages on two
different channels:

	 (send on a; send on b) | (recv on b; recv on a).

The nonbuffered (rendez-vous, synchronous) semantics guarantees a
deadlocked situation. Section 7.5 of a recent survey reviews a few
alternatives for progress on session type systems (Huttel, Lanese, et
al., Foundations of session types and behavioural contracts. ACM
Comput. Surv. 49, 1, Article 3 (April 2016))

** challenges for type checking

[The pessimistic wording in the conclusion refers to an earlier draft,
before we realized the connection with BPA.]
The typing rules (Figure 6) are mostly syntax directed and we
conjecture that the choice between -o and -> could be resolved by
bidirectional type checking.
Polymorphism would be dealt with as in Haskell: polymorphic recursion
requires type annotations and linearity would be addressed by
threading the linear variable environment.
An algorithm for deciding type equivalence would be sufficient for type checking a fully type-annotated program.
This algorithm would apply the construction in the paper: translate to BPA and use the algorithm for
equivalence of BPAs.

For more ambitious goals (full type inference and subtyping), one
would have to define some sort of type unification and type inclusion.
These problems are more complicated than mere type equivalence and we
expect to address them by using approximations, leading to sound, but
incomplete procedures. 

** equirecursive vs isorecursive presentation

We believe there is an isorecursive presentation, which leads to decision problems related to visibly context-free
languages. For these languages, equivalence and containment are decidable, so that it seems to give rise to
easier decision problems (but at the price of a loss of some expressiveness).

** (1) why not Tree -> TreeChannel -> end

That type would be appropriate for a calculus with channel passing. In our calculus,
the typing does not fit the recursive call, even though it would fit
non-recursive calls.

** (4) let

let (a,b) = e in ... eliminates the tensor
let x = e1 in e2   is syntactic sugar


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      doc/ICFP2016/types.tex                                                                              0000664 0001750 0001750 00000100245 13216017506 014626  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \section{Types}
\label{sec:types}

This section introduces the notion of types, the machinery required
for defining type equivalence, and a proof that type equivalence is
decidable. % which relies on bisimilarity.

\subsection{Types and the Kinding System}
\label{sec:types-kinding-system}

% Base sets

We rely on a few base sets: \emph{recursion variables}, denoted by
$x,y,z$; \emph{type variables} denoted by~$\alpha,\beta$;
% drawn from a set $\Tyvars$;
\emph{labels} denoted by $l$,
% drawn from $\Labels$,
and \emph{primitive types} denoted by~$B$, which include $\unitk$ and
$\intk$.
%, drawn from $\btypes$.

% Kinds

A \emph{kinding} system establishes what constitutes a valid type,
distinguishing between session types, general types, and type schemes.
%
The kinding system further distinguishes linear from unrestricted
types.
%
\emph{Prekinds}, denoted by $\prekind$, are session types~$\kinds$,
arbitrary types~$\kindt$, or type schemes~$\kindsch$. \emph{Multiplicities}, denoted by~$m$, can be
linear~$\Linear$ or unrestricted~$\Unrestricted$. \emph{Kinds} are of
the form $\prekind^m$, describing types and their multiplicities.
%
A partial order $\le$ is defined on prekinds, which describes that a session
type of kind~$\kinds$ may be regarded as a type of kind~$\kindt$,
which in turn may be regarded as a type scheme of kind~$\kindsch$.
Similarly, multiplicities establish that an unrestricted (use zero or more times)
type can be regarded as  a linear (use exactly once) type,
that is $\Unrestricted < \Linear$. The two order relations form a
complete lattice on kinds. Its ordering is determined by
$\prekind_1^{m_1} \le \prekind_2^{m_2}$ iff
$\prekind_1 \le \prekind_2$ and $m_1 \le m_2$.

% Kinding environments

\input{fig-types}

A \emph{kinding environment}, denoted by $\Delta$, associates
kinds~$\kind$ to type variables~$\alpha$ and to recursion
variables~$x$. When writing $\Delta, \alpha::\kind$ or
$\Delta,x::\kind$ we assume that~$\alpha$ and~$x$ do not occur
in~$\Delta$.
%
The notions of kinds, types, and kind environments are summarized in
Figure~\ref{fig:types}.

% Kinding

\input{fig-kinding}

\emph{Kind assignment} is defined by a judgment $\Delta \vdash T
:: \kind$ (see Figure~\ref{fig:kinding}) that
ensures the good formation of types $T$, while classifying well-formed
types in session types, general types, or type schemes, as well as
assigning a multiplicity to session types and general types.
%
The type scheme $\forall\alpha::\kind.T$ binds the type
variable~$\alpha$ and the recursive type $\mu x.T$ binds the recursion
variable~$x$ with scope $T$.  The set of \emph{free variables}, $\Free(T)$, in
a type $T$ is defined in the usual way, and so is the \emph{substitution}
of a type variable~$\alpha$ (resp.\ recursion variable~$x$) by a
type~$T$ in a type~$U$, denoted by $U\subs T \alpha$ (resp.\
$U\subs T x$).
%
We assume the variable convention whereby all variables in binding
occurrences in any mathematical context are pairwise distinct and
distinct from the free variables.

A session type may be $\skipk$ indicating no communication (this type
is unrestricted as it denotes a depleted channel that can be garbage
collected), $!B$ for sending a base type value, $?B$ for receiving a
base type value, or $(S_1; S_2)$ for the sequence of actions denoted
by $S_1$ followed by those denoted by $S_2$. Further, there are branch
types $\oplus\{l_i:T_i\}$ and choice types $\&\{l_i:T_i\}$ that either
select and send a label $l_i$ or branch on such a received label and
then continue on the corresponding branch. The formation rule for
sequence makes sure that its kind can only be unrestricted
$\Unrestricted$ if both $S_1$ and $S_2$ are. The formation rules for
the branch and choice types are straightforward.

The formation rules for recursion variables, type variables, and base
types are as expected.
% Recursive types cannot involve type schemes. 
Recursive types of the form $\mu x.T$ require that the body $T$ is
contractive in $x$ using the judgment $\Delta \Contr T$, %:\gamma$,
which we define shortly. The formation rules of the remaining type
constructions contain no surprises; the constituend types must not be
type schemes.  Finally, kind subsumption is standard and the
abstraction rule enables the formation of type schemes where
abstraction is restricted to types by the constraint $\kind
\le \kindt^\Linear$.

Regarding session types, the kinding system makes sure that the
operators $\&$, $\oplus$, and $\_;\_$ are only applied to session types,
that is, to types of kind $\kinds^m$. Types like
$(\intk \rightarrow \intk); \skipk$ and
$\oplus\{l_1\colon \intk, l_2\colon \intk\otimes \intk\}$ are not well formed. In
addition, types entirely composed of $\skipk$ may be assigned the
unrestricted kind~$\kinds^\Unrestricted$, whereas all other session
types are assigned the linear kind~$\kinds^\Linear$. An example of a
well-formed unrestricted session type is
$\cdot \vdash \mu x.(\skipk;\skipk)::\kinds^\Unrestricted$; an example
of a linear session type is
$\cdot \vdash (!\intk;\skipk);?\intk :: \kinds^\Linear$.
%
Recursion variables and type variables occurring free in types must be
defined in the kinding environment.
%
% Finally, a type scheme $\forall \alpha_1\dots\forall\alpha_n. T$ is
% \emph{well-formed} if $\alpha_1, \dots, \alpha_n \vdash T \isOk$ is
% derivable. 

% Contractivity

\input{fig-contractivity}

The language of types includes recursion, hence we must pay particular
attention to
\emph{contractivity}~\cite{DBLP:journals/tcs/Courcelle83}.  A type~$T$
is \emph{contractive on a recursion variable~$x$} if $\GEnv \Contr
T$ %:\gamma$
is derivable under an environment~$\GEnv$
that does \emph{not} contain~$x$.
%
% The contractivity system classifies session types under \emph{skips
%   only}, $\Guarded$, and \emph{productive}, $\Productive$, both
% denoted by metavariable~$\gamma$. % Not any more (vv)
%
% If $\GEnv \Contr T$ % : \Guarded$
% is derivable
% % , but not $\GEnv \Contr T : \Productive$
% then $T$ is essentially composed of $\skipk$s. On the other hand, if
% $\GEnv \Contr T$% : \Productive$
% is derivable then $T$ describes a nontrivial interaction.
%
The intuitive reading is that any use of recursion
variable~$x$ must be preceded (i.e., guarded) by a type construction that is different from $\skipk$.
%
The kinding system makes sure that well-formed types are contractive, by
calling the contractivity system in the rule for~$\mu$-types.

For example, types such as $\mu x. (\skipk; x)$ or $\mu x.(x; !\intk)$
are not contractive whereas $\mu x.(!\intk; x)$ is contractive.
%
The interaction between the $\mu$-operator and the semicolon is
nontrivial: the type $\mu x. \mu y. (x; y) $ is ruled out because~$x$
is not guarded. However, the type $\mu x. (!\intk; \mu y. (x; y))$ is
contractive because unrolling $\mu x$ reveals that the recursive
occurrence of $y$ is guarded:
$!\intk; \mu y. (\mu x. (!\intk; \mu y. (x; y)); y)$. 
Well-formedness of types is preserved
under arbitrary unrolling of $\mu$-operators, a result that we discuss
below.

The definition of contractivity incorporates type variables $\alpha$
and recursion variables $x$ by assuming that they are always replaced
by types that behave differently from  \lstinline|skip|.  Type
variables must be restricted in this way 
because contractivity of $\mu x. (\alpha; x)$ requires
$\alpha::\kind \Contr \alpha$ % : \Productive$
to be derivable so that $\alpha::\kind \Contr \mu x. (\alpha;
x)$ % : \Productive$
is derivable.

By abuse of notation let $\types$ denote the set of closed,
well-formed types, that is, of types $T$ such that
$\Delta \vdash T :: \kindt^m$, for some $\Delta$ that does not
bind recursion variables.
%
To avoid notational overhead, we let $S$ range over (closed) \emph{session
  types} with the understanding that $\Delta \vdash S :: \kinds^m$,
for some~$\Delta$ that does not bind recursion variables. We also
write $\kinds$ for the set of all such session types. 

\begin{lemma}[Substitution and kind preservation]\
%     [Type substitution preserves kinding]
  \label{lem:subs-preserves-kinding}
  \begin{itemize}
  \item If $\Delta,\alpha::\kind_1 \vdash T_2 :: \kind_2$ and
    $\Delta \vdash T_1 :: \kind_1$, then
    $\Delta \vdash T_2\subs{T_1}{\alpha} :: \kind_2$.
  \item If $\Delta,x::\kind_1 \vdash T_2 :: \kind_2$ and
    $\Delta \vdash T_1 :: \kind_1$, then
    $\Delta \vdash T_2\subs{T_1}{x} :: \kind_2$.
  \item If $\Delta \vdash \mu x.T :: \kind$, then
    $\Delta \vdash T\subs{\mu x.T}{x} :: \kind$.
  \end{itemize}
\end{lemma}
%
% \begin{proof}
%   By rule induction on the first hypothesis.
% \end{proof}


\subsection{Type Equivalence}
\label{sec:bisimulation}

Type equivalence is nontrivial in our system because the session type
sublanguage has a non-empty equational theory. This theory has two
components. First, the $\skipk$ and the sequence type constructors
form a monoid and as such should respect the monoidal laws: $\skipk$
is a left- and right-identity with respect to the sequence operator
and the sequence operator is associative. Second, the reading of the
$\mu$-operator is equirecursive, which means that a $\mu$-type is
equal to its unrolling. 

\input{fig-type-equivalence-lifted}

We concentrate on defining type equivalence for the session type
fragment (and extend to a congruence with respect to the remaining
type constructors, see Figure~\ref{fig:type-equivalence-lifted}).
%
% \vv{Perhaps alpha-equivalence? but we want more than that; we would
%   like to say that $(\skipk;\skipk,\intk) \approx (\skipk,\intk) $}
%
 Our approach relies on bisimulation. We regard two session types as
equivalent if they exhibit the same communication behavior. Previous
work follows a similar line, but syntactically restricts recursion to tail recursion which
simplifies the definition of type equivalence and guarantees its
decidability \cite{DBLP:journals/acta/GayH05}. 

To define the bisimulation on session types, we first need to define
a labelled transition system. Afterwards, we show that bisimilarity for
this system is decidable by reduction to basic process algebra (BPA),
a well-studied system
\cite{DBLP:journals/iandc/ChristensenHS95,DBLP:journals/jacm/BaetenBK93}. BPA
is known for generating context-free processes, which fits well with
our context-free session type framework.

\input{fig-lts}

In our labelled transition system, we consider the following primitive
actions:
\begin{itemize}
\item $!B$ and $?B$ for sending and receiving a base type value;
\item $\oplus l$ and $\& l$ for sending and receiving a label from a
  choice or branch type;
\item $\alpha$ (type variable) for an unknown, but nontrivial behavior.
\end{itemize}
In the following, let $A$ range over $\alpha$, $!B$, and $?B$; let $\star$ range over
$\oplus$ and $\&$; and let $a$ range over both $A$ and $\star l$. 
The labelled transition system is given the set of (well-formed) types
$\types$ as states, the set of \emph{actions} ranged over by $a$, and
the transition relation $\LTSderives$ defined by the rules in
Figure~\ref{fig:lts}. The transition relation 
makes use of an auxiliary judgment $\DONE{S}$ that characterizes
``terminated'' session types that exhibit no further action~\cite{DBLP:journals/jacm/AcetoH92}. The type
$\skipk$ has no action and a sequence $S_1;S_2$ has no action only if
both $S_1$ and $S_2$ have no action. A $\mu$-type has no action if
that is the case for  its body after unrolling. The definition of
$\DONE{S}$ is terminating for well-formed types. 

Apart from that, an $A$ action reduces an $A$ type to $\skipk$; $\star
l_i$ selects branch $l_i$ in a branch or choice type; and the
remaining transitions define the standard left-to-right behavior of
the sequence operator as well as the unrolling of the $\mu$-operator.
\begin{lemma}[Progress]\label{lemma:lts-progress}
  For each well-formed closed $S$, either $\DONE{S}$ or
  $\exists a, S'$ such that $S \LTSderives S'$.
\end{lemma}

%\vv{Proof sketch?}

The labelled transition system is deterministic and thus 
image-finite and finitely branching, but it has infinite transition
sequences 
($\mu x.!B;x \LTSderives[!B] \mu x.!B;x \LTSderives[!B] \dots$) as
well as transition sequences that visit
infinitely many different states
($\mu y.?B;y;\alpha \LTSderives[?B] \mu y.?B;y;\alpha;\alpha
\LTSderives[?B] \dots$).

Type bisimulation is defined in the standard way. We say that a binary
relation $\mathcal R$ on types is a \emph{bisimulation} if,
whenever $S_1 \mathcal R S_2$, for all $a$ we have:
%
\begin{enumerate}
\item for all $S_1'$ with $S_1 \LTSderives S_1'$, there is $S_2'$ such that $S_2
  \LTSderives S_2'$ and
  $S_1' \mathcal R S_2'$;
\item the converse, on transitions from $S_2$; i.e., for all $S_2'$ with
  $S_2 \LTSderives S_2'$, there is $S_1'$ such that $S_1 \LTSderives S_1'$ and
  $S_1' \mathcal R S_2'$.
\end{enumerate}

\emph{Bisimilarity}, written $\TypeEquiv$, is the union of all
bisimulations; thus $S_1 \TypeEquiv S_2$ holds if there is a bisimulation
$\mathcal R$ such that $S_1 \mathcal R S_2$.
%
Basic properties of bisimilarity ensure that $\TypeEquiv$ is an
equivalence relation, and that $\TypeEquiv$ is itself a
bisimulation~\cite{sangiorgi2014introduction}.

Two examples.
%
Take $S_1 \eqdef \mu x.\oplus\{l\colon \alpha, m\colon x\}$ and
$S_2 \eqdef \mu y.\oplus\{l\colon \skipk, m\colon y\}; \alpha$.  We
can easily show that $S_1 \TypeEquiv S_2$ by exhibiting an appropriate
bisimulation. Obviously the pair $(S_1,S_2)$ must be in the
relation. Then, using the rules in Figure~\ref{fig:lts}, we conclude
that $S_1 \LTSderives[\oplus l] \alpha$ and
$S_2 \LTSderives[\oplus l] \alpha$. Then we add pair $(\alpha,\alpha)$
to the relation. Because $\alpha$ reduces to $\skipk$, we add to the
relation the pair $(\skipk,\skipk)$. The other transition from
$(S_1,S_2)$ is by label $\oplus m$; in this case we have
$S_1 \LTSderives[\oplus m] S_1$ and $S_2 \LTSderives[\oplus m] S_2$.
The bisimulation we seek is then
$\{(S_1,S_2),(\alpha,\alpha),(\skipk,\skipk)\}$.

Now take $T_1 \eqdef \mu x.?B;x$ and $T_2 \eqdef \mu y.?B;y;y$.
%
We have $T_1 \LTSderives[?B] T_1 \LTSderives[?B] T_1 \dots$ We also
have
$T_2 \LTSderives[?B] T_2;\alpha \LTSderives[?B]
T_2;\alpha;\alpha\dots$
Since these are the only available transitions, the relation
$\{(T_1,T_2;\alpha^n) \mid n\ge 0\}$ is a bisimulation and contains
the pair $(T_1,T_2)$ when $n=0$.

% For example:
% %
% \begin{itemize}
% \item $S_1 \eqdef \mu x.\{l\colon \alpha, m\colon x\}$ and
%   $S_2 \eqdef \mu y.\{l\colon \skipk, m\colon y\}; \alpha$;
% \item $T_1 \eqdef \mu x.!B;x$ and $T_2 \eqdef \mu y.!B;y;y$.
% \end{itemize}

We now briefly explore the algebraic theory of bisimilarity, beginning
with some basic laws.
\begin{lemma}[Laws for terminated communication]
  For well-formed closed $S_1$ and $S_2$, if $\DONE{S_1}$ and $\DONE{S_2}$, then $S_1 \TypeEquiv S_2$.
\end{lemma}
%
\begin{proof}
  By Lemma~\ref{lemma:lts-progress}, neither $S_1$ nor $S_2$ can make
  a step. Hence, the relation
  $\{(S_1, S_2) \mid \DONE{S_1}, \DONE{S_2} \}$ is a bisimulation.
\end{proof}

\begin{lemma}[Laws for sequential composition]
\label{lemma:seq-laws}
  \begin{align*}
    \skipk;S \TypeEquiv&\; S
    \\
    S;\skipk \TypeEquiv&\; S
    \\
    (S_1;S_2);S_3 \TypeEquiv&\; S_1;(S_2;S_3)
    \\
    \star\{l_i\colon S_1\};S_2 \TypeEquiv&\; \star\{l_i\colon S_1;S_2\}
  \end{align*}
\end{lemma}
%
\begin{proof}
  Each law is proved by exhibiting a suitable bisimulation. For
  example, for the distributivity law we use the relation that
  contains the identity relation as well as all pairs of the form
  $(\star\{l_i\colon S_1\};S_2, \star\{l_i\colon S_1;S_2\})$.
  %(VV: one  more case here).
\end{proof}

Next we consider $\mu$-types and substitution.

\begin{lemma}[Laws for $\mu$-types]
\label{lemma:mu-laws}
  \begin{align*}
    \mu x. \mu y. S \TypeEquiv&\; \mu x. S\subs xy
    \\
    \mu x.S \TypeEquiv&\; S \quad\text{if}\quad x \notin \Free (S)
    \\
    \mu x.S \TypeEquiv&\; \mu y.S \subs yx
    \\
    S\subs{S'}{x} \TypeEquiv&\; S\subs{S''}{x} \quad\text{if}\quad S' \TypeEquiv S''
    \\
    \mu x.S \TypeEquiv&\; S\subs{\mu x.S}{x}
  \end{align*}
\end{lemma}
%
\begin{proof}
  Again, each law is proved by exhibiting a suitable bisimulation. For
  example, for the first case we use the relation that contains the
  identity relation as well as all pairs of the form
  $(\mu x. \mu y. S, \mu x. S[x/y])$ and
  $(\mu y.S[\mu x. \mu y. S/x], \mu x. S[x/y])$.
  % (VV: one  more case here).
\end{proof}

% \begin{lemma}
%   Rewriting a type with one of the bisimilarities from
%   Lemmas~\ref{lemma:seq-laws} and~\ref{lemma:mu-laws} does not affect
%   well-formedness of a session type.
% \end{lemma}

\begin{lemma}[Type equivalence preserves kinding]
  \label{lem:equiv-preserves-kinding}
  If $\Delta \vdash T_1::\kind$ and $\Delta \vdash T_1 \TypeEquiv T_2$
  then $\Delta \vdash T_2::\kind$.  
\end{lemma}

\begin{proof}
  By coinduction on the proof of $\Delta \vdash T_1 \TypeEquiv T_2$.
\end{proof}


\subsection{Type Equivalence Is Decidable}
\label{sec:decidability}

It turns out that we can translate each well-formed session type into
a guarded BPA (basic process algebra) process. The \emph{expressions of recursive BPA
processes} \cite{DBLP:journals/jacm/BaetenBK93} are generated by the
grammar
\begin{align*}
  E &::= a \mid x \mid E_1 + E_2 \mid E_1;E_2 \mid \varepsilon
\end{align*}
Here, $a$ ranges over atomic actions, $x$ over recursion
variables, $E_1+E_2$ denotes nondeterministic choice, $E_1;E_2$
stands for sequential composition, and $\varepsilon$ stands for a terminated
process. A \emph{BPA process} is defined as a pair consisting of an expression and a 
finite system of recursive process equations
\begin{align*}
%  \BPAprocess &= (E_0, \{ x_i = E_i \mid i,k\in\Nat, 1 \le i \le k \}) 
  \BPAprocess &= (E_0, \{ x_i = E_i \mid 1 \le i \le k \}) 
\end{align*}
where the $x_i$ are distinct and the $E_i$ are BPA expressions with
free variables in $\{x_1, \dots, x_k\}$. The behavior of a process is defined as the
behavior of $E_0$.

\input{fig-lts-bpa}


%\begin{definition}
  A BPA expression is \emph{guarded} if every variable occurrence is
  within the scope of an atomic action. A system $\{ x_i = E_i \}$ is
%  guarded, if each $E_i$ where $x_i$ occurs in some $E_j$ is guarded. 
  guarded, if each $E_i$  is guarded. 
%\end{definition}
%
%\begin{definition}
  A guarded BPA process $(E, \Xi)$ defines a \emph{labelled transition
    system}. The transition relation is the least relation
  $\LTSderives$ satisfying the rules in Figure~\ref{fig:lts-bpa}.
  Sometimes we explicitly write $(E, \Xi) \LTSderives (E', \Xi)$ if
  $E \LTSderives E'$ using the equations in $\Xi$.
%\end{definition}

The reason for our detour to BPA is the following decidability result
by Christensen and
coworkers~\cite{DBLP:journals/iandc/ChristensenHS95}, which we take as
the basis for proving decidability of type equivalence.

\begin{theorem}
  Bisimilarity is decidable for guarded BPA processes.
\end{theorem}

To reduce session type equivalence to bisimilarity of BPA processes,
we need to exhibit a translation from (well-formed) sessions types to
guarded BPA processes and show that this translation itself is a
bisimulation.

To this end, we define an unravelling function for a session type $S$,
$\Unravel(S)$, recursively by cases on the structure of~$S$.
\begin{align*}
  \Unravel(\mu x.S) & = \Unravel(S[\mu x.S/x])
  \\
  \Unravel (S;S') &= \left\{%
  \begin{array}{ll}
    \Unravel(S') & \Unravel(S) = \skipk
    \\
    (\Unravel(S); S') & \Unravel(S) \ne \skipk
  \end{array}
                        \right.
  \\
  \Unravel (S) &= S \qquad \text{for all other cases}
\end{align*}
The function $\Unravel$ is well-defined and terminating by our
assumption that the body of a recursive type is contractive. 


To define the translation to BPA,
we first show that, for a well-formed session type $S$, $\Unravel
(S)$ is guarded.
% That is the output of $\Unravel (S)$ is either
% $\skipk$ or it has one
% of the following forms:
\begin{lemma}[Characterization of $\Unravel$]
  \label{lem:unravel-yields-guarded-types}
  Suppose that $S$ is a well-formed closed session type.
% $\GEnv$ does not bind recursion variables and that
  % $\GEnv \vdash S :: \kinds^\Linear$,
  Then $\Unravel (S)$ is defined and yields either $\skipk$ or a
  guarded type of the form $O$ where
\begin{align*}
  O &\grmeq A \grmor \star\{l_i:S_i\}_{i\in I} \grmor (O; S)
\end{align*}
\end{lemma}

%\vv{Do we need a proof sketch?}

Now we define the translation of well-formed $S$ to a BPA as
follows. Assume that all recursion variable bindings are unique in the
sense that the set $\{ \mu x_1.S_1, \dots, \mu x_n.S_n\}$ contains all
distinct $\mu$-subterms of $S$.  Furthermore assume that for any free
recursion variable $x_i \in \Free(\mu x_j.S_j)$ it holds that
$i<j$. That is, the $\mu$-subterms are topologically sorted with
respect to their lexical nesting.


Now define unrolled versions of the $\mu$-subterms that have no free
recursion variables. As we are just unrolling recursion, replacing
$\mu_i.S_i$ by $S_i'$ in $S$ yields a term that is bisimilar to $S$
(Lemma~\ref{lemma:mu-laws}).
%
\begin{align*}
  S_1' &= S_1[\mu x_1.S_1/x_1] \\
  S_2' &= S_2[\mu x_2.S_2/x_2][\mu x_1.S_1/x_1] \\
       &\;\;\vdots \\
  S_n' &= S_n[\mu x_n.S_n/x_n]\dots [\mu x_1.S_1/x_1]
\end{align*}

Define the BPA process for $S$ as follows.
% 
\begin{align*}
  \toBPATop{S} &= (\toBPA{S}, \\
   \{ x_1=&\; \toBPA{\Unravel (S_1')}, \dots, x_n = \toBPA{\Unravel (S_n')} \}
  \\
  \\
  \toBPA{\skipk} &= \varepsilon \\
  \toBPA{A} &= A\\
  \toBPA{\star\{l_i\colon S_i\}_{i\in I}} &= \sum _{i\in I} \star l_i; \toBPA{S_i}\\
  \toBPA{\mu x.S} &=
                    \begin{cases}
                      \varepsilon & \DONE S
                      \\
                      x & \text{otherwise}
                    \end{cases}
                   \\
  \toBPA{S_1;S_2} &= \toBPA{S_1} ; \toBPA{S_2} \\
  \toBPA{x} &= x
\end{align*}
It is deliberate that we do \textbf{not} unravel the top-level type $S$ as this expression need not
be guarded. All equations are translated from unraveled session types so that they are guaranteed to
be guarded.

% Alternative: one could also define the equation extraction by
% induction on the kind derivation for $S$ and the right-hand side
% extraction by induction on the contractivity judgment. 

\begin{lemma}
  If $\mu x.S$ is a closed subterm of well-formed $S_0$ and not
  $\DONE S$, then $\toBPA{\Unravel (S)}$ is guarded with respect to
  $\toBPATop{S_0}$.
\end{lemma}
%
\begin{proof}
  By Lemma~\ref{lem:unravel-yields-guarded-types}, $\Unravel (S)$
  either yields $\skipk$ or a term of the form $O$. The answer
  $\skipk$ is ruled out by the assumption not $\DONE S$. Hence, the
  translation of a type of shape $O$ is guarded.
\end{proof}

It remains to show that $S$ is bisimilar to its
translation. Essentially, we want to prove that the function
$\toBPATop{\cdot}$ is a bisimilation when considered as a relation on states of transistion systems.

\begin{lemma}\label{lemma:skip-implies-done}
  Suppose that $S$ is a well-formed closed session type.
  If $\Unravel (S) = \skipk$, then $\DONE{S}$.
\end{lemma}
\begin{proof}
  Induction on the number $n$ of recursive calls to $\Unravel$.

  \textbf{Case }$n=0$. $S=\skipk$ and $\DONE{\skipk}$.

  \textbf{Case }$n>0$.

  \textbf{Subcase }$\mu x.S$. $\Unravel (\mu x.S) = \skipk$ because
  $\Unravel (S[\mu x.S/x]) = \skipk$. By induction,
  $\DONE{S[\mu x.S/x]}$ and by applying the $\DONE\mu$ rule
  $\DONE{\mu x.S}$.
  %

  \textbf{Subcase }$S_1;S_2$.  $\Unravel (S_1;S_2) = \skipk$ because
  $\Unravel (S_1) = \Unravel (S_2) = \skipk$. By induction
  $\DONE{S_1}$ and $\DONE{S_2}$. By the $\DONE;$ rule
  $\DONE{S_1;S_2}$.
\end{proof}

\begin{lemma}\label{lemma:s=unr-s}
  Suppose that $S$ is a well-formed closed session type.  Then
  $\toBPATop{S} \TypeEquiv \toBPATop{\Unravel (S)}$.
\end{lemma}
\begin{proof}
  Induction on the number $n$ of recursive calls to $\Unravel$.

  \textbf{Case} $n=0$. In this case, $S$ must be $\skipk$, $A$, or
  $\star\{l_i:S_i\}$ and the claim is immediate.

  \textbf{Case} $n>0$. There are two subcases.

  \textbf{Subcase} $\mu x.S$. Then $\toBPATop{\mu x.S} = (x, \dots)$
  and there is an equation $x = \toBPA{\Unravel(S\subs{\mu
      x.S}{x})}$.
  Now, $x$ is obviously bisimilar to
  $\toBPA{\Unravel(S\subs{\mu x.S}{x})}$.

  \textbf{Subcase }$S_1;S_2$. If $\Unravel (S_1) = \skipk$, then
  $\DONE{S_1}$ by Lemma~\ref{lemma:skip-implies-done}, and hence
  $\DONE{\toBPA{S_1}}$. Furthermore,
  $\Unravel (S_1;S_2) = \Unravel (S_2)$ and, by induction,
  $\toBPATop{S_2} \TypeEquiv \toBPATop{\Unravel(S_2)}$. The result
  follows because
  $\toBPATop{S_1;S_2} = ((\toBPA{S_1};\toBPA{S_2}), \dots) \TypeEquiv
  (\toBPA{S_2}, \dots)$
  and $\toBPATop{\Unravel(S_2)} = \toBPATop{\Unravel (S_1;S_2)}$.

  \begin{sloppypar}
    If $\Unravel (S_1) = S_u \ne \skipk$, then
    $\Unravel (S_1;S_2) = S_u; S_2$.  By induction, we know that
    $\toBPATop{S_1} \TypeEquiv \toBPATop{S_u}$ and as bisimilarity is
    a congruence
    $\toBPATop{S_1;S_2} \TypeEquiv ((\toBPA{S_u}; \toBPA{S_2}), \dots)
    = \toBPATop{\Unravel (S_1;S_2)}$.
  \end{sloppypar}
\end{proof}

\begin{lemma}\label{lemma:bisimulation-BPA-forwards}
  Suppose that $S$ is a well-formed closed session type.  If
  $S \LTSderives S'$, then $\toBPATop{S} \LTSderives \toBPATop{S'}$.
\end{lemma}
\begin{proof}
  By induction on  $S \LTSderives S'$.

  \textbf{Case }$A \LTSderives[A] \skipk$.
  In this case $\toBPATop{A} = (A, \emptyset) \LTSderives[A] (\varepsilon, \emptyset) = \toBPATop{\skipk}$.

  \textbf{Case }$\star\{l_i\colon S_i\}_{i\in I} \LTSderives[\star
  l_j] S_j$, for $j\in I$.  In
  this case
  \begin{align*}
    \toBPATop{\star\{l_i\colon S_i\}}
    & =(\sum_{i\in I} \star l_i; \toBPA{S_i},
      \overline{x_k = E_k}) \\
    & \LTSderives [\star l_j]
      (\toBPA{S_j}, \overline{x_k = E_k \mid x_k \text{ reachable}}) \\
    & =  \toBPATop{S_j}
  \end{align*}

  \begin{sloppypar}
    \textbf{Case
    }$\frac{S_1 \LTSderives S_1'}{S_1; S_2 \LTSderives S_1';S_2}$.  In
    this case
    % \begin{align*}
    %   \toBPATop{S_1;S_2}
    %   & = ((E_1; E_2), \overline{x_j = E_j})
    % \end{align*}
    $ \toBPATop{S_1;S_2} = ((E_1; E_2), \overline{x_j = E_j}) $
%
    where $E_i = \toBPA{S_i}$ for $i=1,2$.  Because
    $S_1 \LTSderives S_1'$, we obtain by induction that
    $\toBPATop{S_1} = (E_1, \overline{x_j = E_j \mid x_j\text{
        reachable}}) \LTSderives \toBPATop{S_1'} = (E_1', \dots)$.
    Therefore,
    $$((E_1;E_2), \dots) \LTSderives ((E_1';E_2), \overline{x_j =
      E_j}) = \toBPATop{S_1'; S_2}$$
  \end{sloppypar}

  \begin{sloppypar}
    \textbf{Case
    }$\frac{\DONE{S_1} \quad S_2 \LTSderives S_2'}{S_1; S_2
      \LTSderives S_2'}$.
    Again, $\toBPATop{S_1;S_2} = ((E_1;E_2), \dots)$ where
    $E_i = \toBPA{S_i}$ for $i=1,2$.  It is easy to see that
    $\DONE{S_1}$ implies $\DONE{\toBPA{S_1}}$, that is, $\DONE{E_1}$.
    Because $S_2 \LTSderives S_2'$, we obtain by induction that
    $\toBPATop{S_2} = (E_2, \dots) \LTSderives \toBPATop{S_2'} = (
    E_2', \dots)$.
    Therefore,
    $(( E_1;E_2), \dots) \LTSderives (E_2', \dots) = \toBPATop{S_2'}$.
  \end{sloppypar}

  \begin{sloppypar}
    \textbf{Case
    }$\frac{S[\mu x.S/x] \LTSderives S'}{\mu x.S \LTSderives S'}$.  In
    this case $\toBPATop{\mu x.S} = (x, \overline{x_j = E_j})$ where
    $x=x_i \in \overline{x_j}$ and
    $E_i = \toBPA{\Unravel (S[\mu x.S/x])}$.  By induction,
    $\toBPATop{S[\mu x.S/x]} \LTSderives \toBPATop{S'}$, which proves
    the claim because
    $\toBPATop{S[\mu x.S/x]} \TypeEquiv (E_i, \overline{x_j = E_j})$
    by Lemma~\ref{lemma:s=unr-s}.
  \end{sloppypar}
\end{proof}

%\clearpage
\begin{lemma}\label{lemma:bpa-unr-s}
  Suppose that $S$ is a well-formed closed session type and that $\toBPATop{\Unravel
    (S)} \LTSderives \BPAprocess'$. Then $S \LTSderives S'$
  and $\BPAprocess' = \toBPATop{S'}$.
\end{lemma}
\begin{proof}
  By induction on the number $n$ of recursive calls of $\Unravel$.

  \textbf{Case }$n=0$.

  \textbf{Subcase }$S=\skipk$. Contradictory.

  \textbf{Subcase }$S=A$. Then $a=A$, $\BPAprocess'=(\varepsilon, \emptyset)$, and $S' =
  \skipk$.

  \textbf{Subcase }$S = \star\{l_i\colon S_i\}_{i\in I}$. Then $a = \star
  l_j$ with $j\in I$, $\BPAprocess'= \toBPATop{S_j}$, and $S' = S_j$.

  \textbf{Case }$n>0$.

  \textbf{Subcase }$S = S_1;S_2$.
  If $\Unravel (S_1) = \skipk$, then $\Unravel (S) = \Unravel
  (S_2)$ with fewer than $n$ calls. As $\toBPATop{\Unravel (S_2)} \LTSderives \BPAprocess'$, induction yields
  that $S_2 \LTSderives S'$ and $\BPAprocess' = \toBPATop{S'}$.
  As $\Unravel (S_1) = \skipk$, we know that $\DONE{S_1}$. Hence,
  ${S_1;S_2} \LTSderives S'$ and $\BPAprocess' = \toBPATop{S'}$.

  If $\Unravel (S_1)\ne \skipk$, then consider $\toBPATop{\Unravel({S_1});
    S_2} \LTSderives (E',\overline{x_j=E_j})$ because $\toBPATop{\Unravel({S_1})} \LTSderives
  \BPAprocess_1'$ where $\BPAprocess_1' = (E_1', \overline{x_j = E_j \mid x_j \text{ reachable}})$,
  so that induction yields some $S_1'$ such that $S_1 
  \LTSderives S_1'$ and $\BPAprocess_1' = \toBPATop{S_1'}$.
  Clearly, $(S_1; S_2) \LTSderives (S_1'; S_2)$ and $E' = E_1'; \toBPA{S_2}$.

  \textbf{Subcase }$\mu x.S$.
  $\Unravel (\mu x.S) = \Unravel (S[\mu x.S/x])$ with one less
  invocation of $\Unravel$. As $\toBPATop{\Unravel (S[\mu x.S/x])}
  \LTSderives \BPAprocess'$, induction yields that  $S[\mu x.S/x] \LTSderives
  S'$ with $\BPAprocess' = \toBPATop{S'} $.
\end{proof}

\begin{lemma}\label{lemma:bisimulation-BPA-backwards}
  Suppose that $S$ is a well-formed closed session type. If
  $\toBPATop{S} \LTSderives \BPAprocess'$, then there is some $S'$
  such that $S \LTSderives S'$ and $\toBPATop{S'} = \BPAprocess'$.
\end{lemma}
\begin{proof}
  By induction on $S$.

  \textbf{Case }$\skipk$. Contradictory: $\BPAprocess = (\varepsilon, \emptyset)$ cannot step.

  \textbf{Case }$A$. $\BPAprocess = (A, \emptyset)
  \LTSderives (\varepsilon, \emptyset)$ and $A \LTSderives \skipk =: S'$.

  \textbf{Case }$\star\{l_i\colon S_i\}$. 
  \begin{align*}
    \BPAprocess
    & = (\sum\star l_i; \toBPA{S_i},  \overline{x_j =  E_j}) \\
    & \LTSderives[\star l_i] (\toBPA{S_i}, \overline{x_j =  E_j \mid x_j \text{ reachable from }\toBPA{S_i}})
  \end{align*}
  and
  $\star\{l_i\colon S_i\} \LTSderives[\star l_i] S_i =: S'$.

  \textbf{Case }$S_1;S_2$.
  \begin{align*}
    \BPAprocess
    & = (\toBPA{S_1;S_2}, \overline{x_j =  E_j}) \\
    & = (\toBPA{S_1};\toBPA{S_2}, \overline{x_j =  E_j})
  \end{align*}
  There are two cases. Either $(\toBPA{S_1},\overline{x_j =  E_j}) $ can make a step or $\DONE{\toBPA{S_1}}$ and
  $(\toBPA{S_2}, \overline{x_j =  E_j})$ can make a step.

  If $(\toBPA{S_1},\overline{x_j =  E_j}) \LTSderives (E_1', \overline{x_j =  E_j \mid x_j \text{ reachable}})$,
  then $S_1 \LTSderives S_1'$ and $(E_1', \overline{x_j =  E_j \mid x_j \text{ reachable}})  = \toBPATop{S_1'}$, by induction.
  Now we obtain that
  \begin{align*}
    & ((\toBPA{S_1};\toBPA{S_2}), \overline{x_j =  E_j}) \\
    &\LTSderives ((E_1';
      \toBPA{S_2}), \overline{x_j =  E_j \mid x_j \text{ reachable}})\\
    &= \toBPATop{S_1'; S_2}
  \end{align*}
  Choose $S' = S_1';S_2$.

  \begin{sloppypar}
    If $\DONE{\toBPA{S_1}}$ and
    $\toBPATop{S_2} \LTSderives (E_2', \overline{x_j = E_j \mid x_j
      \text{ reachable}})$,
    then $\DONE{S_1}$ and $S_2 \LTSderives S_2'$ and
    $(E_2', \overline{x_j = E_j \mid x_j \text{ reachable}}) =
    \toBPA{S_2'}$,
    by induction. Now, $\toBPATop{S_1;S_2} \LTSderives
    \toBPA{S_2'}$. Choose $S' = S_2'$.
  \end{sloppypar}
  \textbf{Case }$\mu x.S$. \\
  $\BPAprocess = \toBPATop{\mu x.S} = (x, \overline{x_j = E_j})$ where $x=x_i \in \overline{x_j}$.
  If $\BPAprocess \LTSderives \BPAprocess'$,
  then it must be because there is an equation $x_i = E_i$ and $\BPAprocess_i := (E_i, \overline{x_j = E_j})
  \LTSderives \BPAprocess'$.
  By definition of $\toBPATop{}$ it must be that $E_i = \toBPA{\Unravel(S[\mu x. S/x])}$ and
  $\BPAprocess_i = \toBPATop{\Unravel(S[\mu x. S/x])}$.

  Use Lemma~\ref{lemma:bpa-unr-s}
  to establish the claim.
\end{proof}

\begin{theorem}
  Suppose that $S$ is a well-formed closed session type and let
  $\BPAprocess = \toBPATop{S}$.
  \begin{enumerate}
  \item If $ S \LTSderives[a] S'$, then $\BPAprocess \BPAderives[a]
    \BPAprocess'$ with $\BPAprocess' = \toBPATop{S'}$.
  \item If $\BPAprocess \BPAderives[a] \BPAprocess'$, then $S
    \LTSderives[a] S'$ with  $\BPAprocess' = \toBPATop{S'}$.
  \end{enumerate}
\end{theorem}
\begin{proof}
  By Lemmas~\ref{lemma:bisimulation-BPA-forwards} and~\ref{lemma:bisimulation-BPA-backwards}.
\end{proof}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                           doc/ICFP2016/sigplanconf-guide.pdf                                                                  0000664 0001750 0001750 00000313215 13216017506 017034  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               %PDF-1.5%����
156 0 obj<</Linearized 1/L 104077/O 158/E 12731/N 27/T 100836/H [ 676 786]>>endobj             
xref
156 19
0000000016 00000 n
0000001462 00000 n
0000001547 00000 n
0000001695 00000 n
0000001793 00000 n
0000002388 00000 n
0000002563 00000 n
0000002657 00000 n
0000002771 00000 n
0000003058 00000 n
0000008647 00000 n
0000009059 00000 n
0000009458 00000 n
0000009842 00000 n
0000010307 00000 n
0000010436 00000 n
0000012119 00000 n
0000012381 00000 n
0000000676 00000 n
trailer
<</Size 175/Root 157 0 R/Info 155 0 R/ID[<3E6AA0EC231AA6DFAB1A9C9D6D9289AC><866409EB9AB4EF49A0013B161CB331DD>]/Prev 100824>>
startxref
0
%%EOF
        
174 0 obj<</Filter/FlateDecode/I 1146/L 1130/Length 688/S 932/T 1057>>stream
h�b```f``���������ǀ |l@���q��:���xF6Am"��^^F�R*�?�v�%<e`�(�rr�FkS��21,�؞��T}Ád�!k�yLj�����[�+L�[85�K2�ǐ��X��dJ���M���ei�8��49�}O[>[���\���GDO��Ǥ�|8���ew�ׇ_f%:�.j�f۬糺�Iqr����0d��@A����@s�Vg��4V����K�����,Н�Ng��\��!���`�o��;�s��=;���][q��;�!
@����-o���8�Z�A@��(pGu��w�x�T_��n j�qL�����Р8L���:��@���
H8�t��|
�D%%Hl��80�fcc8[PP44M�QPPII��I��cRR266����&��p�p �#���/�� �(:�íBR(�Ň,..���8�A�c9����T�4�XK�I��
<-n��9d�.�N?�@J@cf��A=���ߴ�XCt����f2㖯i]᫰��F��Ȇɶn���x8��s�H8-�b0i�;`ɲ��A����+vii0o;'��@��)ꌲ����~b��툙'��mg�_2�@�Kb�%C
�f`l]1�Q � �;�
endstreamendobj157 0 obj<</Metadata 154 0 R/PageLabels 147 0 R/Pages 150 0 R/Type/Catalog>>endobj158 0 obj<</Contents 168 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 159 0 R/Rotate 0/Thumb 120 0 R/Type/Page>>endobj159 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F2 169 0 R>>/ProcSet[/PDF/Text]>>endobj160 0 obj<</BaseFont/NPEHOM+CMBX12/Encoding 164 0 R/FirstChar 40/FontDescriptor 166 0 R/LastChar 147/Subtype/Type1/ToUnicode 167 0 R/Type/Font/Widths[438 438 375 375 313 375 313 375 563 563 563 563 563 563 563 563 563 563 313 375 375 375 375 375 375 850 800 813 862 738 707 884 880 419 375 375 676 1067 880 375 769 845 839 625 782 865 375 1162 375 850 375 375 375 375 375 375 375 547 625 500 625 513 344 563 625 313 375 594 313 938 625 563 625 375 459 444 438 625 594 375 594 594 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 375 313 375 375 625]>>endobj161 0 obj<</Domain[-1 1 -1 1]/Filter/FlateDecode/FunctionType 4/Length 60/Range[-1 1]>>stream
hު6THLIQ0�0P�-�QH�/V0T�3� ��DR+�3�p(*.M��d�R2�j�  ���
endstreamendobj162 0 obj<</Angle 45/Frequency 75/HalftoneType 1/SpotFunction 161 0 R/Type/Halftone>>endobj163 0 obj<</BG2/Default/HT 162 0 R/OP false/OPM 1/SA false/SM 0.02/Type/ExtGState/UCR2/Default/op false>>endobj164 0 obj<</Differences[40/parenleft/parenright 44/comma 46/period 48/zero/one/two/three/four/five/six/seven/eight/nine/colon 65/A/B/C/D/E/F/G/H/I 76/L/M/N 80/P/Q/R/S/T/U 87/W 89/Y 97/a/b/c/d/e/f/g/h/i 107/k/l/m/n/o/p 114/r/s/t/u/v 120/x/y 144/quoteright 147/fi]/Type/Encoding>>endobj165 0 obj<</Filter/FlateDecode/Length 5502/Subtype/Type1C>>stream
hޔYy\S׶Nɉ�X��S��Y�ުmiUG,�� 3�<�!�N@�B@d"
A�E��Vm���Z}��{+�V��C7���`[��w�{�_�眽�Z߷���Z���B 
mֺ:�x�e���R�����L�!?Ғ�z~�j�x�`��%C�
+_�a�*[5\`!.pp���'��%��'<�vWhȞ�h�p[o��o[�X�e�!�A޶N�mW�D��{aN H-C���0+�x�`��`�X�P xC X*��,��d� X :�b�`���j�t���x�`��S(������$���*/�'t��X�\t����C�����^���Sj5�����mT9�+� �����Y�Y;��f�M�P�YCx���æK�|x��#4#>�6�H��q��Q{FG[��͏��4�?ߘ,��	��`	Lކ�"��/�*� ����4�:S�Z��[��m�Y�I���}�|�*���Y[���y}��E�N�s�6��Ŗ`)�e�L�߇���lq�F�#+V��T�
c�DL-�k�
^s/��X�F�ӢBfo1*�~?:��n���5ΰ�)[�B)L�)2��*�Kli���2޼��-��U(L�+���-��ca"P`���x㯀�E���V�f,\�$U��Tf�%o��\��/���a�Vd�6Q1W���&o� //CСC��v֦Ra���`�`-�K�
� A:N�)+��@Xɐ����,y�MF�袹��rP�.�s��ه�W�j��3	J�w��ZNNHf��w�D06���g;	5� ��>�nP�@v�,B�L��Jr�'��0�_�99���'�b��"Ug��(�����6
O�Xp��"^���$+J隐"U�9�ϑ�5�yY�D__�~ +��xO_=s�1A�Ԫ;���a����1�;;:lt��s8G�SR�*V�/$��܆���/�\�N�p�K4�c;�Â���-�����Q�����9��
���������9��7����Jv��,��"�76���p��>�ek��q�2��spp�@/.�?L�R��ՙ*izRf��rQ5k�$F~���x'�K���K$u��XJP����]�b	�V��!R�s���4����e�?��ؔ
G!ƈ`6_N��ԣ2��C�~�k�:~D/ۋ�j�Ha�mIvc�yUl��,m�@)q�bPru�VS�ϖ�v�a�Yo9�	x�5��s=%�kZ��w�n����{��+`��.��#-PΙ�l8V�[�]@�/��R��I���'؎��C�2U�YᵜOf|�n�o���}��Ώw~2��t�e�c~�l�v�5<8z5z}'B�1�:�D~&�h@7
d0������)SћLI�#�oe���D��@"�A�}%J<m�V��稊T�ROI F�	�z��H�[�5*g`�K�]R|���KS�n���F�u�%yylI9*���R�<{�d�0�@���CxK��}�~){<���e�L�P��5o�7�8o�a����wo���r������P�Av�V����^��u,�7�)�yc��c�p���f��T����9z��l��N�_&�G��)�F��	U���/�bru�fY�n}`X@�������_C�y%8�����0�`1Xw|Pv��Œ��Tt��.���%�p��Z� �S��/��� ^�W�=���¹�&���7=��o]�T�`�U�Jk��)�*4klVx&�X�D��c���_E*=g�"}PQ(�]��ڜ5t���^<��k�EmL�OHU�gR�.O���q������^b��B�D����Uѡ����i���w�`��{�_x)�`b��[]��n��,ހ?�A+��:��Nގx���i� <(T������)�t
#l��U:�2
π�����ͧ�(;�]�(>涢v	���l<	��^0O86`U��IHIA�j�L����̪y�g�8������~(��u��~�Q�1�:����Tm*f�H[�z>\Fd��`�#�W	ƶ�1.�`=L%� 
τ�ؕî����^�Nݽ�� ��U��2<(a��Vv�ߊ��a�Doa����L'��E�F���Ӣv9����Ӓ�^�p��)Y��9�u�r��QZ��2�C)��w����Ħ{�����{q�<r�U��o`�D� t�鹇T��1NR�w,0�4m�v=4�	̫[���3��W�Ux�����¾���4JT�(�Y�i�rr��DE�Н�lRtP�'�nj�U��H�'`-{\q�H��L8�P��^�U����|����r��kgO1B�q ��k}��ĵ[����"m'�+��|j����t��<#���R5pF;87�<�l�?O���"$&���~b,�%�)ȓ񜂴����^lIv�>BL�#�j%T[�킴`�oE���/�Y�_�+[:c�Y[^C�]X��Gd��U�ߋ�S��'&n�4V�ĺ�T�`���$R��dU���Xf�r��h?��+s��v�Pv�������
�������T0��E{9h�kLf%��'��iyb.�v|�S#������߸ �ŧw����:]���D38��뉁����.Vi�%L-��s�M�F�ٗ�j+�5���0���2�o!�N���(��k�Ԩ#e����c�>���6��Y�2�$�oK�Ϳ�y��w�"�� ��?�0��^�E�W�x�9�1%�L_�v���~o+ɁW�6�!���2�v��Ƞ��0�q8� `
�ž`��`wA;*�"�j_�[�1�a�T���NNSs^���B��@t���4��B���^����'[?m��|tm�[]\�S��we�O���^����`�w�m��N�fp�,�V{�mcܓe�-�t[�)2���e��h57��v#�}�w�VÕ�G�oc�_�+OL���E:�F����͊���E���@^�����[���oK+Ҳ�b��$5�9#(*�Y�<?�K�Q^�)��4��X���u�z>~��Q����3���oE�ǯ���d])�uN���݊G�بl��bt([ρ+U���Y�TV�E��|h:�'�3W�n-M�oڟ�dϣ�bUM��$�!�]&�z[��\#LNc+�(;��BY{�R��LY����EE%uu�m�J���Ϟ>��˷XXG��p������&5��)�h�B�߁����o�Bڍ�$Y>��n�t��s�����E���.��+�<?������4����!���B|�'L~[a�L�}����es)<�)��m�r���I��W/N��6
n�ӕy���ٸY��1!K�6���ߠD5Re�r���ux9$��88�0A��knS�X����3y���N#�g"m��-��"��f+k����\�����)�7?�ڛS���ڴ�e������	钂܂ϙ*rf�z̄:��Su�Pr�T���{�'��&�&��'#�nN��m�X���Z�O�mDm�9�-ě��B.H7�W=�7[@^��@���K�`;7l����9���A�V���+b������IQ����A}��;:��]H�#�-��]*J��hP�^����Yb�x�~����6�^f���'O��L��ݷ��K��r�L����*�*�ʫ��O&f�}z�hȸY��q�[cȠ�׃>v�xe� <b�v�.b��5%�Y��XM"����dAz+?�U�F��妒����d��V+2���sl�`����N�2�j�J�ee%%��<[��tm%]�0��;;�.�¯~?���&���d#�0�L]� �����������a[<���0�dW�����\��rQ�Cϓ��[z��{�/�_A0�.�AMw��t��i��a0!�s���Vpk���&b0���w�0Z����׷�t+y�tH�O�ñ�c{�3�<�(K@��ꬤtnU��(ORs�k�Զk�Z��Uj�̳l�Yc2�`��Ԋ��a���=�� ��D�~,y�Y��H�V�	)�qQ��ͱ�jZ�:�};��c�����ȯ៝I�鯓��p�j9�|2=�m�����͐�"l�Oso�`��Î�"�#�'���Ҫ(��ks*�Ҭ���$YTi̾}�U���fO�WTl[��]��`v��NA�^�\\L���L�ߥ�g#�]����~����O\�潆�b��c5�Ǆ�@�N��h���kʊJX�K�G�E��E�۶!���)�A_������=<��=�v�,*�lH�Z��$����f?����z[L޲,8�U�����#3WL�}`�p
7z��E�������L������Cn@}"���������>0�,�+I��6G�%�)Q=����m~�)���z����0�0�'qaqQ�E�څE�]L�ۀ��f�e�g�T^�'�y9�W$&�_ �Q��e%5�0ۏ�KC*�,%%�da�gdaw��C�4�\bvfvr����������r�d��|��̼��c�o�¾;����Ǳ�����;���V336�xDqj��`��N}�ϟoAx��ك5��������K>�1]����7�s T��{��[=���d�C</�?�£�ς����CRҰ�G�Q�"
R���9Rid>l�Z�U��M����MF�F�d0���}�I���3��D�u��j?���0���0����;��X�����|;}�`O�!��Ӯ$c&9����k��L���K� WO�<6��7D҆<����de\���WWg�h�޴��-gOS�$x�����<^���1޶Q�v�/�>��C�z�tJM`3���Y���]��L0�����JdS�QoH�a����m���ה�����jM܋6�h������W%�;rU2'aX1�=f���Z�s7JW�[tL��=�Q�S��M�䓹�ܘ�ۣؒ��N��'����f���x"�yJ�S=�N͒��M�ݭ=W�u����xn�V������%F����S~�oƝzg�Z�řu'Yx@a���J���yV{�4��7$���jc�B2t<m�i�]����$�*�Ő�C��HA}M�R��)�1-ⓦ.�	�fB�C�0N~��e���1�=S���.�^R!|�)���m��B�L�iC��*]�RE~*n���0D�a:~99>x�y�e
��:�Sĉ
�r!C"�c-jb:�j?����y�����D�{�f��<	����h�V��QXc��4~(]������K�5]()++]�e����F3���q5C>G�K��EbH�b���@g'&S�r�9����Mf��ED�o����։k�դ�4������`�3� x�{į�3��Gqݪ�pQ+�nd�R�"�d��#Z��m'�4K�L<���{I}Y�gp��r�~�����l�FQܷ����j	ޖG�:�X��� ��X[�Po=Dcm�'ɮ_���o �S
endstreamendobj166 0 obj<</Ascent 689/CapHeight 672/CharSet(/space/A/u/t/h/o/r/quoteright/s/G/i/d/e/C/M/S/I/P/L/N/l/a/parenleft/parenright/c/n/f/m/p/g/y/v/b/eight/comma/two/zero/one/period/W/Y/F/D/E/x/T/three/k/four/fi/five/six/seven/nine/R/H/B/colon/Q/U)/Descent -199/Flags 262150/FontBBox[-296 -251 1139 946]/FontFile3 165 0 R/FontName/NPEHOM+CMBX12/ItalicAngle 0/StemH 43/StemV 109/Type/FontDescriptor/XHeight 441>>endobj167 0 obj<</Filter/FlateDecode/Length 328>>stream
h�T��n�0E���Y����(��"i#eч��{0C�T�e�"�;M�p|�;s5&���vX y�9��`;��t����`!U�f���ی���̇˼ื�Z��6��_`�K��$��C?�����'	��s�8�]@B]C��H�ύ{iF��m���TX���S��k�ƞ�z�AW��v��Dmo�/�2tRJ���1�q^�q�"�$��i`j�w�ւ�\+V��c�E�
�B��º	��YJ���$p�,��8XΖ<�)�R�����-EK\�(lH(E�I�e���1#�o�6os���"\o�8�z�x���x��� �&��
endstreamendobj168 0 obj<</Filter/FlateDecode/Length 313>>stream
h�L�Mn�0��>���T���,)mQ��*�t��!���Ъ��;�	E���dޗ�r#p�ѽE�'�9�{$�Z(��5׊Ja$V��Fph<!�xm��%�kp�?�M�Qdym?�({������P���Ne�Kl�p&!%���U��</�^��,����0�Bs;Lv$�_  " ���%�6�c�U�����c_5��ܤ�]j��B)1���kM�_c	B�Bv0�}�;�j˾u]t�w�dٝ�׾횘��*"ڮ��,��z&,1�����u^�:��d��W�c�����Hn�?��Q0·m��0 �<tJ
endstreamendobj169 0 obj<</BaseFont/NPEHPN+CMTT12/Encoding 170 0 R/FirstChar 46/FontDescriptor 172 0 R/LastChar 121/Subtype/Type1/ToUnicode 173 0 R/Type/Font/Widths[515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515 515]>>endobj170 0 obj<</Differences[46/period 92/backslash 97/a/b/c 101/e/f/g 105/i 108/l 110/n/o/p 114/r/s/t 121/y]/Type/Encoding>>endobj171 0 obj<</Filter/FlateDecode/Length 1596/Subtype/Type1C>>stream
h�l�Xu��68�t$v����*�(�D�|��@��'��L2a�c���ؾc'N���� �I>�=Q�<������X�������|�miГ�������>���X� �q\�fm��k�ħd�d��݉�)�4��9��1�y�H���5����10�!�z�of�,����)��r���dj�Z��Qk�$:%���)�
��<��R�B��-ʓ�&HV�˵[�~ñ[�a�l� ���L�`��롇XlBv�b�2���H�)a˴�.lF�6&���^�.��&�A�`L�Fh�G,�艌�l��N���H�a�~�'}%�6��V�]������EI�1�?������u�=:��jgE]S�����fE�ZmU.ʱ���[���/r�9�������K�+��}�,��l�51�V���D��{��r ��9��x�q]C�.zCժ�4��Y����?�U�a㻔�z{KAM������8KjA�BOWm�������O�9 ��-\�`4����!\�B�Z�t����ݠ�9<���Z_?p��%����=z΀1�~y:Y�S�&��D���������V,Ͽ6L��������.�l���,��6���Ӳ��eTb���za|��2�v3��fS�Xa�3�P�麟AZ&C�EH4�B�;7�Þ%�!�6;�
���tu�8%�:�����ޚc ��w�-��"��j��ͨ�SI�?�1'����~u%nhfD��#|��aF�+B�)$�F�u�ÑAӆ���@?m��������@��%��ӯ̉�)���W4��8h�x�YB�*�p���C�	���C'�P�1�S�uX��mL�ӷ� �ùϾ�2�U���6��a�.j�5�){���j���M�\ƭ*�i�y��˭ʞF�i,�)޺Oc{���^�'�Z�ь[����P*P�ِ 7l:z�����?�K���2��97����΀�q�[��ԷP ȓs������8���8���9YV�cY�������r4��w�;hD��7D�o��x�G+(��A�%���M�&�C�Iק&�AD�L]'VlM""�-N�Q��ж�?�E#�IBqB
�N�<2x����4ڮ���)�n�� tB1."��y7��ys�i��1�Ν��4ڈ�Ȕ�K7FϜ=w�LzB|��F�Xοg8T�M�%'�P���8�;���I�����7.�-����BXב����_�� ��.��4����:s�7�,-^/gE��Ϟ��F�Mf�@��=�9���|?�l��2��D{�ȁ�B�ʓ
�=�_�[uG����쐉��&����PA��7�a)��YQ�Ay�}78정C6�Q0�����\w1Y�G�޾��7�7�6Ѻ���v��;��������H�BeM��ī�~�ަ�Jzq�D?�FV"�L�c7��9A�j(���L�Z-�{����x��BȿT�&ڼAђ��`"�]�f�-��F���Ά=��hz�}��^J�xds-0�����5�����h�6[,-{����Z��J���2�� $mZ$�i��v�rW�Jkm����)���u�=���� z���
endstreamendobj172 0 obj<</Ascent 607/CapHeight 607/CharSet(/space/s/i/g/p/l/a/n/c/o/f/period/t/b/backslash/e/r/y)/Descent -227/Flags 4/FontBBox[-1 -234 716 801]/FontFile3 171 0 R/FontName/NPEHPN+CMTT12/ItalicAngle 0/StemH 58/StemV 65/Type/FontDescriptor/XHeight 428>>endobj173 0 obj<</Filter/FlateDecode/Length 279>>stream
h�T�Mo� ����9n�JW�&ċm�H�����T$��}���� 3��m����軛d���(��:�p�A�(-�}g9
�wۼ�ؚ~�	�����68<����S��p�?�<�Vk�qD�@u
{B�a_ň@C�;o��}�7��VHt��a�:ՀF�?#E����*I�Yƞj���qy��_|�xx��_���U����/s�T>��<(T	�f	������.�����7;��w*�	Vh����/��0 ����
endstreamendobj1 0 obj<</Contents 3 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 2 0 R/Rotate 0/Thumb 121 0 R/Type/Page>>endobj2 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F3 83 0 R/F4 82 0 R/F5 93 0 R>>/ProcSet[/PDF/Text]>>endobj3 0 obj<</Filter/FlateDecode/Length 1094>>stream
hޜV͎�6r�S�(kE�%�:n�6@��V@A�D[L$R )/� }��p�?Cy�4�������7?������Y��4��l�ٺ9�꤮�j��?/fU�TE���]��U���q����۴����st�R�f[�}��@Q$��4�!D����y'?ƹ3�M�f�6:$(=rr�Bi1�k%�A�خ������D�p�I�����J�`c��M��.�b�i |���բ�n��g�a��5i�Dȳ��+�����f��ό]���~�� <�y���!N��C�e�d�PP�k�م�[�6!�O�ޖ���5��0%�Q�V��N�3����|���ɄŠ�~��p���]��6o	wU�\�`���dd-?©�c��F�1q�"zՋ���̞[}`���	}|
@�1H6����`y��E�b6���>)�I��$��5�]�i`-�Np���~:�S���tς1b�F�p����Yߟ��/�a'?귕�=e�1Y�g\i9�<Py�K3�d�)L~��';S�+��������JS��g�{�[{�aQ'Ut�5�A�"A{��R��4��,�s[�,��svE�D�f�<�`2�2�^��Av��ǁ���#��*TylF���T'qK�ŋo��H�S��T�ʠ���XVwX@����ҵ-����90r�����7$nF��kῶ,�C������$8r9���ҡ�����U��;x��t�f�6j��+�N�y,�w��z��r����Y��<M˿�4��q��]�d�+��l�ō;�%���Y3~�B�V�@��;*���I�Ѽy�:74�䫂�,��t�5͍�O�ffQ'�<e83�-¶��,�ZE��K\�WvsK;�"��p��&�u�v�R��A���G |h�h T�+\�����v*�Z�Ы�i�b��a@�w3�NuQ^\מּ;6Rb���N�_)8k�!ix
`��7��͜z���	�nI[!�[�Vۉ�bB[�p��v��������;6��И�?R��[Йn�J�bK� (%'M���\+�	h�HQy�̐��4�P���<��z�l	�,M+��5�Rn��O/W.Ъ#�ߛտ iɽ
endstreamendobj4 0 obj<</Contents 6 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 5 0 R/Rotate 0/Thumb 122 0 R/Type/Page>>endobj5 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F6 97 0 R>>/ProcSet[/PDF/Text]>>endobj6 0 obj<</Filter/FlateDecode/Length 1000>>stream
h޴Vmo�6��_�0�"%R⾵yi3C7(�`���9�!�X�/��HJ4-ˑ�t	b3���sw�������ٻb����x��ey��1��k3�3N�4�f9�O�x�������z&%�����,~hf�9�0��-���c���F���.����a��`�/U�"��<E9�jM��FtaDJ�ŵ�B �cB	>���$n��\�`b���&���qĒ<� ���}���� �gu�="���f��ڷ����_���WѺ7n��{ ��~�v��u�{���ZJC� +r�G��)"	��d<)Ǟ�f��fR�d�#�0A��ԛz9�9�A2�#����x�m�Q�e����'k4�0NSA�T��_�[$���1�T��и���ېTz�U�X}��ĺ�˿CJ�J��P:��Q��߶�.<k7CI����lq����q��'#jsR#*i���OH�@5��zv�@�F�'�N���J�b�r���;����m�v�P	%�����n���U��,W��:H��:�&Ҝxu -�Q6r[�r��0�O.��(���<�7r�T�ʻ[�b��������bsB�;<F��^�ƻ�?(����Q��8G�
�t�ǨPG���P�|R���oJ�k���|E��M�9>�����C�Q��T\	%�Ʃ�����/�ף���c�r�rvV�_�j��w����d��3Z���e4��1��ph��NM�N�jj��J6�)m7]ۯ�z��q
��Ԙ�z�q!��u�}���25,�^���F��[[
�Ĩu!�f�@��ۥh��?H����4���1�t�R��;M)�)}'ڪ�；�<̂�� ���V�j�_�}�����F��+�1��9�tBԓ��5�/�׭�(=W��S��A����'~n�f6��j��vd)BN�46�1!Z8�Ss�ڃ����>�?��{ƒ�����g훀�:����������kvS��` ��F�
endstreamendobj7 0 obj<</Contents 9 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 8 0 R/Rotate 0/Thumb 123 0 R/Type/Page>>endobj8 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F4 82 0 R/F6 97 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj9 0 obj<</Filter/FlateDecode/Length 389>>stream
hތ��N�0D���,�/�u�Z��$̪�HK	ڢR����&6���D��d<�3���%��K1���QU�2��E[J:�TY	�:SZoA�#٬�eCW�l�����<>���MЃW�"�xP���Y<<������5�K�6�s\4lЎ��E����Vؚ4:���~O>3+0�P��J"��:�춝7\h�A���4O�?��֝��`й>?�����kZ���9��9��-�G����H=��,x�s��z5n�}}	YC���������L��#7ƀIO�n$~�~�N*�����?�N�e3]��,�M��_��FQ������j�E,ۙ�����,����Ut*��zR���b���/(�S�#�?�Y=�%ƀ�U1ԝ�a,� 
#��
endstreamendobj10 0 obj<</Contents 12 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 11 0 R/Rotate 0/Thumb 124 0 R/Type/Page>>endobj11 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F5 93 0 R/F8 109 0 R/F9 115 0 R>>/ProcSet[/PDF/Text]>>endobj12 0 obj<</Filter/FlateDecode/Length 1792>>stream
hެXKo�6z�_�����")RRo��$u���ȁ�����Fo|�O(�?��S��kF8����7���%�����rv�G8Z�f$A�%���8�(��X����$Z�ϲ���gW��F���4c1�[~��ey��-GO�H��,%��U|��)F4�,`D�j,�6��ɻ�zK8�Ơ#��4�Ң���	vx���tN2��l�ğ��������Ϟ��s[�U�����G�Ka}��>͍/�$i�(I�DN���_~���%�8:A�ԫ�?���֛Y��Ky�W��ò}����0�F���Gm!������zj�ɝ����f��z�f(Kp��z�j i^�t
�fI\��Jv�)�W��6K)+լ{4_� �^I_���MXY�A��J�)�N�v�n�u�U#���_��ve�0�6./�4�2�.��VMY���2r{�8EI�jh�;�V�u�^ưSze?9�v�a���v	��~>��{�h�G{@�)���j�#�~D6�pD�N�"���"*���vF6��g��?t�߇$�	���fqJ
n 	V:W�a�H_�K/��.�TY�E���Q���]A�4��ڛ�8�J��
��{��բY�#�3��4��S��̕����{���0*P��ky��Q�c��ѹ� rHfμ�<�,�`-J��]l�Vy����3X�T�P�!!�oT;����Ɓ��vO�\u���3�\��M��T� ׮w�9e�/]�蝒�ed��u�X}S�,��Px��']�V�&�$���O�n�A���9�e�?�t�,C�d���Z����Nka? t$�n@B����M�L�kV��=��1�{��،�����A�#�e��eW��a�E:-�-�*�uKq~A��b;�k>gZ��(�V�*0�N*�a�v�֖��QU�3��q+÷��T���[U+�y~��y,�4Jp�0IYDp�g�y(p0���>J}iO]��N�))���tTb��
F�S�δddӴ�`����h�IĶx�u�R��i���c��!��zr˰�C�Y���z/+��V�=��:Q��ڭ��0m|g��?e9��.���p:��4�#�iaI���r�-Ç�<k/9C�:mĺi���Y;�m��a
���&� �8�.���GO�|Tu�����|�{SN���6��\e$��Ӧ��;n��<�4@E�{�n��ό�_�on�GP|-�����+%(����K��"w�@��L���\s�?)?�	Y{��i��ԅ%���T���X_EU�N1J�Z
3IQ-&�!&v2�S��W `��ff�n�"ᓝ��]H�����վ��Z����_�+�,>C:�ip�OK�e��J���ܢ�[���KSN����N#EN�3#<_`LX��O���yAƾ�������@h�(��{~����*�ԷSq��O'�CÑ�Cه��P3s�a.b��(�	7}௣�f��瓽
�p�����H��g�*�ୖ�V���eo�O��xa�b�=6��,t���L�'�)y<>*lY���8�;N����+��3`�]�:v%�����J<� &J>�W-��D�2��N��h�?�w�Jr�bYj����6��z������r�$��np)� 	�nfjh�����zsch燿���1�������?�-�i�ۚh���X
2�Km^Z��t�=T-���c�j��#W�N �������0ϛU�m���h� �����#�Mb����
0 �@�b
endstreamendobj13 0 obj<</Contents 15 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 14 0 R/Rotate 0/Thumb 125 0 R/Type/Page>>endobj14 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F7 106 0 R>>/ProcSet[/PDF/Text]>>endobj15 0 obj<</Filter/FlateDecode/Length 551>>stream
hތSMo�0��+8BUl���t����TY���w�+Vƛ���R�p��6�r�V����7o����9�Ģ��2�1;D���ƹ�9�i�(���6aR�1���<l���D	I�/S��x�ʜ4�������˯��1͊�@E�Q��u����=�dAW߶���>����:��)QQRb�x����iI5O�(I3�I��4��yi�M��G�n>ƍ�>�^�u?�?�v{��p��ê����=m�����4�Y�����E$ +�DB�:�����}�zV�r�7qg�p���
%�g����Ǚ�v�;)�_mP��<��6R��Uc�^�@;������r��>��]�g�,�q/g=���;�^[���hU�F��4�!;9�V8���{���=�+��m�^�'-� >M�Dy���D�1F�ì+���Z�^z �+AK=��Ļ�}X�r��m�=W}.q�;��k�Z��z(�w"=��[�+u����ҟB�z��l~�+���[i���r	^�RdH����Rb>�i��4=�*�Y�W� �)9F
endstreamendobj16 0 obj<</Contents 18 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 17 0 R/Rotate 0/Thumb 126 0 R/Type/Page>>endobj17 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F3 83 0 R/F4 82 0 R/F8 109 0 R/F9 115 0 R>>/ProcSet[/PDF/Text]>>endobj18 0 obj<</Filter/FlateDecode/Length 968>>stream
hތV�n�6z�S�(#R��i6�h�.X
�{�e�VW�TIny�}�9$'�vm���������>|f�nZ�Z-��Ӏ�v���,x;�&9)s����$c���"	v�j�u�X�7{9�j���B}�~_0N���h��g[L�צ���Q���xP]b�h��ط��ػz�1HAD�rc׊�,/r�d<C�վ���Ӕa����77j��f��Ly�y����p���'�qrӽqa�����:���� �y$Q��� fe�M�	�w��3� ;�P2^Qt��qn���j�T�,�ф�"-�Vd`�g%���rZh4����{0���<�[�$Bn�J�"���0��:������\��' ���~k�99��GU��z�Y=�߅�y��v �$�93�9�\^��X]��(��e��<�������T Ӏ�Ah�����纕Ӵ������/�������>i��$���
�p��W�S'y�r�;[6�t�����T?�8܍r�7��/K��Խ��jU�=��l�8��L�hp�vMwrɼ�fB���%͍���hx�ͩE�~������㋥j���0f	�MfƋ�<�y�07yn4?<xX=�lm��hK����sY�����
R$�@ť�<MVB�v�1�q��DI�����>|�������vB����f�6փY�g��]<�r�X�uͨ� .�AϜ�7R�ـhzPȡ�u�?]��O߄d�j�ä,2z�u��8�"�V�u�x�c�t8�N�)5���ԱES���(�"uT�qg���4��n5P2���/��|<�z�뫮n{��1'����(G�V�Q�G�W�Q���l�т$<Ϟ1:��W7�Ԇ蘠�aZ��P�65B�qʡ�5bX�F����(���i�߷��uM���g���a��'��ٝ�M�(�|��P��ׯjt+��E	�0j�1�T�V� �EF
endstreamendobj19 0 obj<</Contents 21 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 20 0 R/Rotate 0/Thumb 127 0 R/Type/Page>>endobj20 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R/F9 115 0 R>>/ProcSet[/PDF/Text]>>endobj21 0 obj<</Filter/FlateDecode/Length 1844>>stream
hެX_�۸��У\��"E�/E��������(�{\�^�bK�$c�(�)�;�3�d����$R��7�g���g=t��/g�~����YA��eQ=I��d�Q�g�2	�v�$z�����8��|�;�͈,r�dy5�����������"MS�ƌ�RJ��W��z���%>��i�������[�<�ZH��DD����2�w9��[�~����������/,��ӵ����r��;���#�ηK��������ȣǈ&�Mt�l�f4��ٿ �QE�D7�Q��v���������?���i7#4�RNDƭvo�(�g ��ku���/(KXve[�#�<� 2g��1�c�#0�Ƅ0��b�;S{�Є�4��S��'�Ѯ �<�����j[ܫj��>m�c��n��'�C� D����GL�c��p`�X_�&�.뀋�Y�蟊K�M�>i�^Bw�67$��G�=�-H�r�q����~�2%<G~{U:�_�B���eKd�o1�W�R���aa/[�$O��ܷ�A�
P}�U�F�}A�ۦ���O�N�3O&���Z�Ъ���`�I��O:4�IVn��7H�}�,�d$R�	��-	��=ziu��p�q��rHe��z�n�i�=���.��ƿ��`��o�t��]�+]�zuF�/�)"l����ǠWno�z��o��q����*N�$k�X�g��X;�o�jwх�Z9�{fcǓ7��Ҽ�׆��.rJ	������@�N���P�VV�y�q�k�1~�¢p�A���S�Բ�0ܩ���g��%~�(�͡�XՁ,���áո>�=p����'�P�8���B9��cWX7��'������5usN/S71��[7!���<�=#9�r�M�яa3A$�r�l�Om���]nz��.��Y����Ҹ���Ȯ3�v�k�ȇ�����yw:,>�ҵPn���n��Y�{��d��l?i�2I?���2��m��-}ꦌH�0o���!��o]�Q�����$C���x��92T�V��^;��1N؋5�}w_�ʴ�J,$�8�I}����R����a�� 
N;{�HK���y!G���*�?���޷U}��s�Zd2�_�~����!�S�M@^�Xζ��6:<��5��@�~��23D<P
�cȜ��0 ���j���=�zwJ��($����J�;t6c���(�B�l'��xA@��P~o8�:*��v�T^��w��_������I�3�Xx:��*y*S7z2�d"�8OAZ�4���2Ϙ+ݴq��ـ�����w3?0�+9�~��Z�le�A��<G*��'�x�qX���'�pEn`� �'v��_Ow
�e�#I�|�4o�aT����n������L�� O1��}���c[Y}TRAC��T��A�u����M��z��	9�ƗjL�DJl�������w_Eƪ�D�2��/>�Y"!ާ_`r�����.N-yN#�1�#m��H�����VԂʳ�"�;�<�&���>����;��Kb�ʜ.)�
��r��Օ-/*2�p������p{.0=���S��wn���۸NY����Y:��;�m��v�|Ϋ����hR.�
6nE ��͓ꟓ$|3����U�o���L|��`�>9�$\���J�ҍ/�$��m8ج�a-(�P�Ǝ3�) ��k�(�C�4�k�TO����&��_�ގgj\�Q�G�r��<2�̘��
��/L�IA�0P��BW�q���|�5M^��ɍ�P���CS�AEם�p@Qan�_!�F%�Wjxi4먲��q�:�݇��J�� �+��
endstreamendobj22 0 obj<</Contents 24 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 23 0 R/Rotate 0/Thumb 128 0 R/Type/Page>>endobj23 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj24 0 obj<</Filter/FlateDecode/Length 756>>stream
hޤTMo�0��W�� 2��q�h+*��$�m�l����e��T�� R�0��N�b�h���?ϼy3'�,;�u�Ǆ���K Q\�?k2�@���0�[�Q��/��߭7�9H�8�A����4�JOT��� J)C����� ��B�AF�$e���4�D~6�� �"csூP�?������%���-��]�v������έ�Մ{8�A�3�w�������̫��`��(�DO�;�nD�&��9#�+�Zx����J��s�%g���pP{�c��Tޅ��p��rƑ��B��'�B�P�Q2�ۘ����5v<L�R���Y�����D�)�oWE���_VzT����"� �D#�O����P���vz���ޭ�p����{KA���I��J���y9GW��gEk!�����bQB�'�up�l�"��a��ue�H��{kŸ�~K"���{�ۯO)��A��2.$2�k��κ��n7ng�M�O{���*��j��'̈́�O�%�b[���y�,��6��E���)��䝔�)������ ��^�lCԢY����$���R�Vnڦv+d%2�3�&����v<��
g�7m�.q�*�͘Ze���5�F�X]��<�)o�:k�N�������ˋmnj�)$�uk�}U��U�
��|w��g��W�ƭ���;�0]����락&�u����X5Ȋ��^W<d�(ϡ˲�t#۸��Mې�ߨx�  ��k
endstreamendobj25 0 obj<</Contents 27 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 151 0 R/Resources 26 0 R/Rotate 0/Thumb 129 0 R/Type/Page>>endobj26 0 obj<</ProcSet[/PDF]>>endobj27 0 obj<</Filter/FlateDecode/Length 8>>stream
h�    
endstreamendobj28 0 obj<</Contents 30 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 29 0 R/Rotate 0/Thumb 130 0 R/Type/Page>>endobj29 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj30 0 obj<</Filter/FlateDecode/Length 972>>stream
hބTێ�6}�W��."��ߺ�K�A���>��@˔�D&U^⺂���ܡH���@��j��9sfȻ_7񴖓_����hM��$Q���4�?oGa���*�f�E��pZ�o[�������"b$�l���ܾ��)Z,��r�+�0N��lKUK���<�P2õ�1�[�^JV��YI�"�!I8`%̖���d��*.XQ���/PVj�Z�U�g�D�,BQ�F�HC�<H�P(��Y�|avqOd)�n{dV���X�[ZWƽ�	�@��$7`���EzX�W�:Y�\�+-��p��v�G)J�<���E�0E�d��=�Qg����>��eM�1��|�հ�B�\������/���NI�Sү��v��W�<�U�Q�,W=|FJ"%���.�w�0n�Z�P�[a�
k�VX�c�u�41g���/=��%)��J�i�z{Oe��u��R��)��aN�-��`i���$��])��)��������^�PQ��UDV
�_x|�Lÿav��kF��Ae���7Z�;�m)�T:����G�&Yw�nԉ`���ћ�=V���l��� �� ���0s�?F��S�	�T�9\������1ژ�\Xc�~\��~�HE1t����A<r{�qeh��wA+2B\ ����OL�� Nk��f{�l���!p�#�$(4Ƚx
�+~k�k��K������+f �-$��4~u��'ɘ��9�\|���]ǅ�7\���N�>l=�fJ���V��碐zg5Y7��w�K�¾�=q<7�RG,��q%P�oi��j�u����@U{̕���Q~@�{�p͸T����oiܯ�$�K���oDH�N��&�;�=bQ�.%�<"{=�ꏪ\��,|����AWe����e����z��7���k*�xW߰<j�ڑ���GZeF�,��y=f�i;�O� L�HM
endstreamendobj31 0 obj<</Contents 33 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 32 0 R/Rotate 0/Thumb 131 0 R/Type/Page>>endobj32 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F3 83 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj33 0 obj<</Filter/FlateDecode/Length 1588>>stream
hޤW�o�6���"��P1���>���<tI�xC�Efl���QTS/�߰��w䑔d�X�!Ht<�������mt�l��̏N.���x�xT�"��� ~fɲ<=Φ	���G��~���t�M'�#G		�,��;��ӛ�����1���L�<��ԛ���.,�f2U��I���:��չ#oo��f�(8�p���%��~��ݝ]_M>���h�o�R� ˕i�����D%�5U��~D��@��y.Z)�J����D��LN�4J=ى�E�w��*�>�J�\0�H�@�F�(�k�,�!�cb�=M�x�E���e�]����F�/YL-E��V��5�qGKװ�T��X�m{���`�*%]r�}>'˗�]�ߕͲ+��}�J�$.�b�g+��g���:�a٘H\��g�h��@X
n��cJ���U}�$�v7;���Q�n�2��y�7���>��'�}�b����Ɯ��Tl_��?���2D$�	��,�%fiWcI�L�L���J4��c�Hm*�i��ެy�b]J�����7	#�
�J�\!$�2��5�z��+f��k�S�����z[\��2$�k��[�|�lThE�7`�v��`�ď�9��H��)�®+�A��-WHJw�~�i]�H���J���u\y�#ȥ�k�Tյ'�M�������@�9��%��  ��)�1zb�,�����'��Y��l,I�PQ���u�X0_�Hb0t!�$�3~((wVhG�у!��A�^�^���.|������X���$U%����~�W�th����q��}F��$	�Ј6�!GZ��#��x/�!��}���N �i���3,'�.%y�.{Am�u''P_�j*���*�$�d��KvžQ'�Ïd�� �gVꆅ,흡����	9�%K�j�E��٨ܼ;�)O|˶����#!%����<(b8�L5u$UDH@���
�k*߰�$K7L���)8ޯ]C��� }���
��%G-~�Ei<|��5��F����q��Hޣ�~B0�S��E�F���B�C����jK Ŏ󤈓�0�
2����砇�=�!+_���`K&����4�<����69�7���L$d��#�0X�()�(�~�d?�Q0,_\��P�1�(�}�j��5���Ԇ
��� j��+�����/iZ [;�8��I�yv��j?k�Mm�R��i��������ɕ�p0�gE�k�u�S�s��L�H���^f�-*,S����C�}q��5ґ�v�M��;6���Zi�Jԏےf	�g���;���:5W� Ֆ�+��E\�m؎ُW�^ж����|5
���I��Ԫ�����~��J�b��ǭ�B�Ϲ3 ���EmF�u���x�D�Z.��=�e���j�w��&�=~8N<1�?̏�a�"��v��� }#�[�ěazA-�}���P�#�v`אN���c5�����h���h�5Z#�Fk�T�c�@F��`lYN�R�1�pt�$ �X�ڵ.EM;rv;T�C_�@�)%z�q]
�ŖW�e�]S���a]j�F�"�]�� G�=��%������? ��]y
endstreamendobj34 0 obj<</Contents 36 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 35 0 R/Rotate 0/Thumb 132 0 R/Type/Page>>endobj35 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F3 83 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj36 0 obj<</Filter/FlateDecode/Length 1573>>stream
h޴X͎�6қ��G���IQR{J��H��(�"��+��Ȓ!�ME��/\r�CI�6I���3ÿ���#��_���]�z���^��p������u`�81KT��*Q,䱙v\�;�7��>��o�l���%��u���v'�`b��$�}����~5J<PHAh��^�6�6��ڑm�Y�؎���+�u�Ws�˦&�����q�D�/��o�|�ɶ��M�����z˜7a���B�sU(�����U��v=���˾�ot]��?8I�$�/QD}�w��Jw��q��b��(H��8�������N
��f����%u������i{R�Q����>�]��7��l��/����� �,��$���Ԗ5��!Ɂ��cT'�Om�G���M�_�!ar ��*C0S��:\<��Y��9�$�uѴGM��&[�.J@+�ăߦʋ�o_�z���lr{�w���e}GP4T�>o��t?�+��.�դ�m��G3�'�b�]$Rq��=�D����d��݅!�6�M��+`���D�X��d25Jx��g��Ġ�h��A��KT2�]G�n��s�,���B��R�"%�4NtQ�n��iMW�w���� ����xDsn���Xb���`*�	��tq��d��"&�J�Z�s���J��[	̘:]>@܃4IX���I���ܳwA�$�j.Y�u~���#�7.�.~�����'��z���>�[���0�F}9��$:��ɲz�_j��GԞ7-�x�ԃr��`���C������E��A�C �#��t@H�e����(mo�=�<�<�e�,jC�4b�%l�m�KvE�\�d����'a�����������} ��,DĔ"�,r���ܛ�F��$,B�5R�		�4#ᅀ���(x��1��{�n��1���c�z�vQФ���ƶx�} \�"��|��q�o�I�2��+G|��T%�:Ƌ�ig�@}xq~�eu���7�M�r�s�p�"^j�8?`Q��:�� ڸmۦ�}'���,�^�,)P[���d,�T�s�=���4��� i��\\7�/�L<��sד�����	�:^J0��`&�7g*I�(�LÞE�͵�>{̎��J͢~�E�h���1�f>:Ε����na���YU=�o��06�C�5���0�;	���%݅�%��;�qE&3��J�1@av<�Y�:���>��d������n��0��4[G����X��c��j5[:7�a��� ��]$��R1',���|������4�i'8�;	�F]~ҭ�s��,��h}�q�)�5�O��1F� *@�7���1&Ȯ�rps]�d�ş����D��q��3���χO*��y�e�X��q�t��NL��m*35�7h�ʎ��!i�Q���Ԁ��.��~3�o�|{u��}C��-�3�z���_���/A�"�l8�4`"��$6}�z� �����k$
m���T�Ӑ�l����c(��ÈL'�����Q�G�k�`�N婲lFL���zA���8{d�w�|A*�Q�[�x�,���_عoN~�bv�~��_f�� #� (
endstreamendobj37 0 obj<</Contents 39 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 38 0 R/Rotate 0/Thumb 133 0 R/Type/Page>>endobj38 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F3 83 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj39 0 obj<</Filter/FlateDecode/Length 1656>>stream
h޴XKo�6z��أ��=��]�q�&��R�9���X+-$*�"�Op.��rHJ���A�g�����gW�d����|M'�$�=��Y��iҏ=�I4�Ә�AۖG�d?Y.��?r|o�}%	*��Kb
*�WG��?�/���S7C:!��I����γ7�F|9M��i �ά���%��ޝ�������/^�w�ߚ��wӏ�_��GL>%!�u3CW�R��4�W� ���A伞�B�8|�����;�� ����4[p�B8�MU5�����F�7�%����6���Uۭ��ㅡE3(����0|Z���/$G	u��fSдf#�Ţi����I��,kcuf�[x-1F��8p0���|�8�a���)���Ĺ�VH$�/�%�-I��׻�+�������z�W\��qg$N�X^ ��-Ji��ݰv��v�9;�U+��X�N�������>&���O�-l�8��ƔMW���!W�Y,s�m�b����r��8
#р7�#�kY.xk�]o��Ӎ�!:8Xvw�o������-.���6����'gCvX`���u���^�- F�AD<Xڍ� 	"�7�҈����=���Q�Ɋ�LV���l{"M�r�3@3�~^~�ҁlyQ
��B��q�����w|�	�9�� N�`U3�O^�ŖL���Րƀ>��zD�ߩ�(�!�'ТHK�"�,���vfo����G�BI�u��w�
(l��l�c�v�[��h�ꑳ��g6�	k{{h��j��.�7�c��:���3��2Nd�w![�RJu�W�h�u�Nׇ��tqV3��s�6�r\+���y���c���������ϛv�ݿ�%��[�˛i�C��[5'"��$��m��%жQA���Hȯo��d���*ŢlQG�V�3��Մ�-�_?<��ܷb���0����fH�GQ��E�$�hɮZ�#}��.f�k7$o˕h��n�Q(����(Ӫ7ۖ�0��LذE-��(W��49(pwz46y����7��GJ`�R��# �ySXq�{�	{�|˨=H�aq�1��P��Z�V��hKW;�+�-[�i`�Y1*k�'h�rHH��"� f�({}�CZ�����7�G���&.+�:�1��|o��`��uQ>X��/�`ee!6�P=��cݱU�Cg�.YB������!�5 ~�&�`�$`�x�k޲
�s@}�m'l����W���q˞꧉�C7���9jVe'v���CO(�iF�tt���E���8�y��y���E���Z��FXG"��7��& ��@7�\�ᷝ��/�ޝV1Fd�b/'�z��wʥ̶��e���|�gۓf5�w|��r�@R�W+��'Y?N��>~�������@���?�PwFh;	���9`Bk+L�>��IT��!+�pR���h�F(}����yS��ф����@�q-�$�f���rAa1̂Y93������ǗAٵ��[v%ټZ����2g��t�~��h�-�O����,�xd�B�%�=�K��.��#�z�3S���!�e��;xZȇ�?S}��_
�85[�'9�)
{:�ԓ�����<	)�y��e�q��C�����Y:���{&�gO������_ Ӂ4
endstreamendobj40 0 obj<</Contents 42 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 41 0 R/Rotate 0/Thumb 134 0 R/Type/Page>>endobj41 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F10 117 0 R/F3 83 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj42 0 obj<</Filter/FlateDecode/Length 1908>>stream
h޴XK��6z�_ᣧ��o��tӦ)��c'�`Ӄ��̸����,����.%�����nSA��)����<u8�~��n��������Y����Cr?aY�N�%�)k6�s���R�s��vC�����)�܋�x���W��^�,t�~�f���@�o}���ϻ���o�c��s�t�r�r�����eQ9���ںvw(�x_2��֋��E�z][v�����k���dK� �ѢNCϴF_��E���w��/�̹8������o�S9�_�M��:N)�p�l�F�	����T�����l�bZ���I�I�M{�^Ȧ)�=ؓ�p�})���΃��(��@��4S��7كN����A+M�w�^\\^m�/Γ�7�q��$�st���×��-���]u<s����,qůcQ��̭���@���sZ'A�E'q��P6��33?M���%hx�����46��i$����S,���!K�<b���]1:y4���5��N�JgNEO����⭸:�;Fq�����O��ț�q8��i��X{ Y�$�=ѓKq��<�x����I�ݩ�	W!a)m($Qҟv˚A��`�q.&H!ުc��^ �}6�-�c�%��)�� >Z3dg�f�L|<��
!�`ī����	7yY���o{ ��1_���\SQZ>1�C��^ �j��1��~<���hε$xL�d��1�*x7���)�V��*VǾ.m��Xj<�(X�e)��X���U ��*2�u�TX��)���݋���(�ɢ�Q��qƾj�+2�X�jx�fT�[	W��?wgƢ��Sw a�UӦ*��-�cI4�wk�A���-�g��V�@��#~ZdM����Bv����u1�"�	9c�D���+��^R���RX�;#�����y'4������x�-�!U��sc��&jq��Ut�xU��B�c	�P������J��Q�k�}�((j�7�<e�ox&�3�F��5�-%$�<���/ey���̩�.�	4�%�Q����/aOpEZ�	@�R؍�%�ql�EgŮu�tp�aN��5w+�R:@�����
�hM�=�|�O�����f|7}dz*M����N��Y�����}�2�;�Q��C��v, �K��p=X��g�,��,�F+Z���8`xDT��m�-eD�V����|_�\C�j�0b[Q'y��:~��A�_<U9�`�������%(?54�/8��?�(O`Z�a��3�4�� ����LϚ�a�^㻐�IM[��Lk�I��!��i��L��j8-�x�B�(���{mZ�\Ñ5�0�zq���J6��?L)W��K{�uQ����z�����F�:4�ͺj��^�SP����T�3A�8)&ۉ\Q���*�cτ�k������,�1�����D��b��T\��%e<�C�H�w�C��P���#�#OȒ4�lf�C�K=��i�Me���)1_���K���@��a������c�ǙE.����,��������Jכ��yO�?<�A��ah�Ӣq�"��Y!!�IDs�'
3��ĝ���v����h���ϙ�fٝb���'59�'���<��z���Dܙ'�X���oPN�be2~��ث�6��[Q~k�ǦS�rOK?��/|�,� ;��vo�C�E
@�QNe})�c��c(��Zf�D,M� ���A��:}�������>�c� ڭ����X�g��Џ֛0~j�t9��I�����*Ui��Tu=}FŤ�]3�I,ͻ
F����+�Q�	Ao�T��xv��픩����X�^��g4���$�I\�d���. �j�&�i	�\�t�? _l��#ձԬf����\�#MX ��v꒦Nj/+V��������    �
endstreamendobj43 0 obj<</Contents 45 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 44 0 R/Rotate 0/Thumb 135 0 R/Type/Page>>endobj44 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F3 83 0 R/F4 82 0 R/F5 93 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj45 0 obj<</Filter/FlateDecode/Length 1306>>stream
hޜVKo�6��_�CR�bH�齹�d�b�@���E�m�֣z4�����;�z$�Ŷ�pH�Ǚo8<�p��f�C<;9��log�x0��EP�?��0´|F�;�ũ��0����`�7�����n�Տ˫���v\�%���	ÐDV��?���+;���ͥ�C?��xߋ77���'Ƨ����W]^������5�	01��<�zW'3�p4?",�y�[@��Ƹo]���
�e�[-�}k]l�:Oڬ,��o���CWo��ڎ�\X����ՙ��H�F������_��"���A�j��,h@�&�M>�]�JA�ڝՈ�A��K��'��~:��v��oJ�Qci��ͭ1>d����	�ďPgq'._s�8�(�b�8�e[!Z#?d���%Vɮ�5���x��Z��q�<���Z�&���,I����_sb��#���V�;�N�����1-��(V��!K�iz�kTl�Ԭ1rZ�yRl��S� �՟;�x�$o��?҄乻kD+�]~/�'���48W�3C���a��>!#����yBCgA"�y2�n�3��{�	�#%�H�RfT������$�������T}�؊�G� ��*�=k����J�61��c�������
���t�K1-+<�c����ƦJR|"*����tY-�aǤ_5�m��&�xI�W�,TQ��غO��j�Ϥ����P"�����S�$↭V��p�M��iHh,F4]J�4$K^(����:�]e�5�ӛ�P^�T0bJGK[�e�� l�8��	�2d�59�Ska�z7	�8�~�T`�ww ��i�:��L��e���Rs|h��w�V�j�RW�?WU���<��(��=�����G(�'�Pxa�$m��?b�uB?p��PJ#3��1�]K�ʯ����sj������uъ�P!K���PV��}��uE�W) ��$��������z?��^U�� 4�+�(�k;$L�$Csi�\@�Z�!����QS1U�k���y��G�kڃ����O�Z��땁��<h�$��v�c��3=���P�22Ex�5�a͝��f�@�@��n����;J@B�w>�����I��Y]�����u�Y�U>�"��$s^]�0���F���y1��u���1���&\��zk��I#F���e� ��9��x��s�V�5��g1��s��@A�z��M���Z��C�GJ4��n����:|��f�]�?��uì���i<�W� �5^	
endstreamendobj46 0 obj<</Contents 48 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 47 0 R/Rotate 0/Thumb 136 0 R/Type/Page>>endobj47 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F3 83 0 R/F4 82 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj48 0 obj<</Filter/FlateDecode/Length 1019>>stream
h޴VKo�F��_����&���k�Ѣ�+���Y�DZؒ!�M��?����R�y8�7Y�(�$�Ϗ�n~�Ģ�i�}���H#U�+����2�q4�)T�"Yp"�ȣj�ʢ�������6~���Y�I�s����$/r��,tqN�r�u�����S���=%7zV�R1e�Y��%�9��nJRQ"�&a9)b�27zj�n���N{���ݰ����語]�^�0���{sh�lkCD���ޚ��(��E�{������*������φ��8�z��͔�!��L�r�s�
Z`��IJ)��'����ONU������L�HPI2��EQ�&�>ë�:@ AH��݄z�@� &����y�O��
<��?�ʢ UD@K�&͖��l���^�vD�Y=��GĨ�
�;�
�$UeI�'W��������Y���a�jj �d��C���a���w����@�XI���怡��q�zCNʛ.�Ja�K��W��y%%a2���9�(=�$��["%��k]����8�*�k�,���¢�/,}a���a}����$S2�8��ǳ� �vċ)A���xL�� ��� ����nna���!Ϩ�V�h4��4���
��1<��7�#��,e{��Gm����c��ch���B�|�_�j� ���ʡ	��_�\��@��y{v툗{ˈ@�1��jf�&��3���>U)�~�#y&dNc�x�� ts4��g��\�����&���x�x��S��;-���ߙ��?/U0�[߾��`ϳ�=�>����͉�Z%!A��xZî1)|pm����q��;I%�n���;7���H�\��c��kO��1�pA�~�nT��&ۻ�r�����l��<�0:�B0G��~NR/-̰���E86���F�Ӆϙ,�\١�Ei��v���8=C��漍�x��z���-~WIa>�[o��.��"q^�eKV�,�|1��Z�+� �9z
endstreamendobj49 0 obj<</Contents 51 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 50 0 R/Rotate 0/Thumb 137 0 R/Type/Page>>endobj50 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F3 83 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj51 0 obj<</Filter/FlateDecode/Length 771>>stream
h��U�n�@�x�W��F�ֻk���R
^Pk	����\����R!>��Of�q�H��5�3���9;3>{{�f��{�{go���ko��Y$>Ƅ$��Ɍd@���Ƌf�����a�_I�I�@(8��ލ�8��_^!�`?A��)��Ż˫� I�b�9���$0�,!B �4���T��/�A�� ч��e7��E[l�b������JN"������>#I	�����Q#��|B@t-�v׉B����7M�*�-BUK5��E�'h}[ZT;���Y&��m�LK�s���1�#�qO���^���d�1׼��f�SGj��U^D��Ӗ�����Y��Gx]	�L�>l"����G[Z��e)�KQC�m��HV��VZi���tm�4�d.9���d64+y�(�{r��(r}��r�u�u�}��̢���4�U��F .`��tYpL��,k��J�DLL��h݈�A!J��^�W��z��_hA;C|zb�A��VwN �^�v���,�|�+��%Y��Q�P�������:T����b��Y�^�Ňv���b�<�r�M�X�������{�&Cr�o	&�-��ۅ10���;Zf_��t��Ade��=j�O%��.+�#m����3W��vm�����/d�iۜ�������tk*�)I�'�54�ͪ����[�h[����Y?ND��m�W���������KJ�#/)��)��*���ق
��V>������n��7�[��4���3�/s�  Q�;P
endstreamendobj52 0 obj<</Contents 54 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 53 0 R/Rotate 0/Thumb 138 0 R/Type/Page>>endobj53 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F8 109 0 R/F9 115 0 R>>/ProcSet[/PDF/Text]>>endobj54 0 obj<</Filter/FlateDecode/Length 839>>stream
hޔU�n�0��+t����.��.I����:z�m&VA[�$��w�KrH�F��M�`����§�_X�0��v��[ZТ�_1��b� �7ɔ(ܨ���XQ�~E���m���;�n���ˊ���[�q����B�(/�8ǊS�cݡ�z�7�{c��JN1G����$!�S?��U�J��bFdSt����0w����[���ULQ�����u�]?� G��ոA��ph=���;��u��O1{�nkބ�Qd�5��>;�mC�ώޔ���03.1i����:�)*̨�#�&��"ޡ�	�������s���k�`�TJ�%ҽ�k�/��3��X=娿�I��E�B�P[�����-�yHLˊR&a��<U�֘֔��:dBL2P~�|~�Xe�5��~8�I��	,����}���Hn���F٧$(���iև�������\�e(ܣX�w}���f�ށ�n�P�����A\'����%v"!�!7�da���n����$���~J���eC�}�|�A��٘�ن��W��T��`X�%����I�8� �W ���4����fz<�d�f��]��|a�|��������;���}�Ja��y_��/�<<� �D}�J4=,Nz(h/�,�ٸ�?"�q�'����3�qػ\��oFm�2Q�v�x:�EMa�?C����VT�!�H�����g�
�^�9+��U�X!(�
A��©�����k]FO���[tA�SZ�p}��>��pXX8E���|���O`{��TQ�!uf�������F�/@1n����ѭ�è��t� �9S�>v�U9fo����M��-� ��
endstreamendobj55 0 obj<</Contents 57 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 152 0 R/Resources 56 0 R/Rotate 0/Thumb 139 0 R/Type/Page>>endobj56 0 obj<</ProcSet[/PDF]>>endobj57 0 obj<</Filter/FlateDecode/Length 8>>stream
h�    
endstreamendobj58 0 obj<</Contents 60 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 153 0 R/Resources 59 0 R/Rotate 0/Thumb 140 0 R/Type/Page>>endobj59 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F4 82 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj60 0 obj<</Filter/FlateDecode/Length 1029>>stream
hޤVˎ�6��Wh)5G�DJ�n�v�}�v�,h���X�!ɝ�A�"��^򒔜��_��އo^�a�a\�P�n�hD��Y���x��4��2#y�Qխ�� ��6�V��<OjH�Y�c���~^��eA���+ˈ�(7\��]{�j�Ӳ�#��MF�X�N�do�r4sC6�	��` �KJJ�$���a-aq��股vr��zhw�ڮ��YzШ�@993��t�ӌ���xϋ$��so^�|���o����y�x��v�M�5%,�~m�������}���g/��xNV8�sLs��Y�<���
��"C��ɚR���<�(',���ԣ�����
6�����o~Ў�u�O�}��8�l�xlu��3�Dӭ��,�F�x�ݩC�l����ꏅ�a�V=��]���!ɚ�8���~�5W�8����s�m6}�ӗ~/�'<��Szx�C�෍-�>�u�>ʾ;�iq�I���8�n���:K��G�R§+8�ذt*_j���mp��NG��)lm�A!f�tX�����cN�/lz�ć��&ݑG)e"�(��zh�
6�g0�".'���r�V'3��n��ᒫb_@�3�_��F�]��˧=�=��;$VK6����r���d�Hh�/�J�ڵn���]frR�'�f�]�����h���!)M���������(�6�04�#��Ql}Bl�2�8�(�����'m�j��$ð�wڙ�Ud��ѣs��mN����'���s
>�
V�2�J���x9���q�Hr�B�tr5�a�������<Z3��z�f��Uf5Ű�n{�ȩ���zr]�)#e��%T��tk[���Kh������@��It#L�k^�-a�����&.��}�����{�4�L:#����Y����%1X��ĵ�:�	H�e\^ᠵ�\�f�֛�\	����s�6�3�kZ--2Ѝ��2�j�o��+�PN	�B�������~������^�F;� �M�6HE��j��  I�|
endstreamendobj61 0 obj<</Contents 63 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 153 0 R/Resources 62 0 R/Rotate 0/Thumb 141 0 R/Type/Page>>endobj62 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj63 0 obj<</Filter/FlateDecode/Length 581>>stream
h�lT�r�0}�+x�NQ�؀sq�t:�L�>9}�A6jb@ԓz��]���N�c�{v9{V����K��G�{W�?���[�U��~c&y��X�y��$- ��b_\���$e�I�IH\�
��w�&��r����a�eʂ�QQ��~<�_l����5qX���7��_���ܑ%�RT�y�J��[�g�Q����p:"�NJR�G	J���h�.���Jt�vR;*EUß'�jǨ��hoPn �H/�获��"\��d -��Q�bg4Tc'�7m��jH����5p��\���~l��0�'}�f~аi7�.���A�Q����*�`b���:&k39^��S�nK�-�5\�%6�J��Ż�?���.HT�S�ٲ$���� �-�r����s�0qg�Ώ�����N�v�FL�v<���$�����}o�P28E����5Sԟ�D�\�wcO=K��NH��k���=�d$�!-�@��:�:(&L#�xy@�*��ń%j8����Т��͕��A%�h����Nm���Dt�SmO!JVh'��q������h�Βr3̓��1�f%��v�k(�G}����+� �C�
endstreamendobj64 0 obj<</Contents 66 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 153 0 R/Resources 65 0 R/Rotate 0/Thumb 142 0 R/Type/Page>>endobj65 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F8 109 0 R/F9 115 0 R>>/ProcSet[/PDF/Text]>>endobj66 0 obj<</Filter/FlateDecode/Length 1541>>stream
hޔV�n�8��_�Gy3"u�[���.�6h���-Ӳ��(PRR#��B�wx��z����Bg�gxy{O�E;���._�1�7#�8&�؇_Kc?Fi��q�(�d<ߍ�q�\�{-��-m:&&� ��d�a�누(I�m�`+P�H�Zx��*y!h���^Y�|j�2��$V���)F)�mh�-��4�B�y��$(��cZ�fm.��d�t�-�r���
���q�4�5T0�RK�~��{��pw�^�x�$m�M��י
�O1"��ˑ�y2�@H5�$I
HMM;8[�F�.�/pR���+ڶ��MW��r��Ct-�����)~�6������m��t�q�(Πpp�=Wc?���5��8{�G����k�V�LI������h��~2��Z�9�΄"��Y��-�n����.�����2�f�wU��͞cĔp����EC\�����l�[1ў�s��6����5*
�:�ɀ�:n��ș[���wL��38 #�S�߻����71N���?Rʴ�\�G1G�D>#pA+���rc){��
�V�hoa��"A�]a�-�+�Y����ؔnZW���C�$T���p�� I��vP��^���v��r��YZ}gm��8B)1��AU��u��?T��OL�X� e:�������ړ����V`���=:\�����4��(�ڎI�ft�_@ X�̷ܭ2���Y��걬*K�9`�6�!��� ��{�e�	��O� ���T�v�*�#¬ң*Ax2ŘDލ��r�U��T)���H��d�@���NFdl�(m�e�k�j���s{7�z��A����v��8(1WE������sc�,��i���e�W�̇�m]MU��ގ���f�L#�,	R��!q#?	���e����]U�\c�Q��.�?ծ�܅"�O�Q�
����c'\xj��VLvQ�b��f�e��YJ
V3hPLs�b���c�m53��/h�2��헺���;�F�US��E~.���/̋�5M]��˲�����GF~�!�V���� m7 �\����Ϝ��@��0���n|P��}���b/�]� �}2��8U�.�+V���:����	��2���m�l���(�Z�Ŷ3z�͚*We�v�kA�-]z���Ds�V�����������_x�H�-�I���Y.k���x�Q�wv�f�}c�v���fz�;u��MmM�]�}z���~v����.>By4��f�{�P>��7��r_ӝ|MHfF뢇��>_�~�w|%��\!a@�1l�Zມbͪ�ԫY�7����Y/'Vw��o�����x��)7�4���|��^x3���������~%O�po��9��mY3ڛC�J&��x/KV��E��S�����]?_��n�,4펋��(�7|�T�X��u.'�M!����N�&)�Υ�W�`4����^N�����LT�0.�<}���Ϭ����`��[�,�5m�غ���+�]��m�]�|Z92�z�ͅ=j7�q$wD0i�&���� ���
endstreamendobj67 0 obj<</Contents 69 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 153 0 R/Resources 68 0 R/Rotate 0/Thumb 143 0 R/Type/Page>>endobj68 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F2 169 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R/F9 115 0 R>>/ProcSet[/PDF/Text]>>endobj69 0 obj<</Filter/FlateDecode/Length 2354>>stream
hތX_��6��G
?�71W�$J��&�������7s��<�6m+џ=I�fo'����> �?��5�� @�� ���{�<v�������2Xn�LdJ��-(_(��K�*��Uy�����~�I���$�H��8Q������{������_V�0E�%b�N�D��˷/z��7�\����Շ͏�1ּ�Z��W	-�9iX$����Ŷ,�c�ߝVҬ���{7�rϬ�i��,ݔ��n��Y:o�g��ѵ[�90��;�o�r�`��03yϝ[r��C�jT�_�!�^��/��I���[8��ٝ+]�_i�5R"�Xl�]SUy��*%��^5uw.�����L�d-��"JdI��[2��H�X.U�
�\�w=�����*A$���"�jc�1�Y����z�\�t���|BP�,|�K�y`�'z�u�k���hj�d4[K�k.�}���� ���DKP�O�k{��E���zA��"
}�T���f���R��خ9�m~<�}[O�l�$���Ղ3E�H�]~�t�#�V$ݙ�b$o�rNȌN�L������MԐz�a�Ψ7^^5g�N#�f�{�frR��]Q"����T�h�������(@CT�H�ئ"��Ժ3���nX���[�NYԃ��{]Y��7�t�Yc��m>���0�!�O�m�u=P9R�i��
E���7 �r�{����&1ڂ��*_'���|��n
��Z�a j�x��%�]z�O+�=����3y!� �� ����_�( ���tY��Q�}��K���c{�Q�cݑU���=rF�J.���QHZc|{C@�8
��7Ψ6PG��ǻ2�wM}��~�.@˼3gb�\��{7��E���xn�����ї���[�x�9 ;?
ulV��l�5E�=���l�>Z@(u< < ~=5I,T��v!2����+�j��x����F�i�S����ܚ�B���H޵#�^���4��҉O�{HǤ|�X�y���'g0S�`�[D�Ȱ�KJ�T�}8�5�;�DdѶ�ԟ흏����4pU3 	0In��o�{����D3�{sP���+#~�اq��<k�k�$mS�<3[F�cE�OG5�����cܮL���4�ᩳ������u}����f�g@h8=n�ǅ��o�c��z�!�mX����ng��1�;?I�H�J���N8��� ��粙���2�X�D���0�#6@=Ff�9sǈE����?5�)�ǶL�'�AKYo@��6q0�'�S�x7nqeݘ
o�Ldg"f"Y�yr&�B!y=����M�|@��xH�IS������������8�̝���� 1`�*E/�")i> �;Yt��A7��M�D#`��c����9���S���h��sgm�%��񶬺-Q���RD�ڸ'�E�CF]�{F�bV�IUc:)��h6M\ٳ�*X���0sK�o��g8��8���/F�ҁ'i���R�؇���J�l��AX��B�t�k���RY@����o'anQ35nR��g��^҆����T�EU�}���t[ܵz=xb��%[�^���,}���2]�����a]Z=�Uaߋ�#���E�8k޾>>OuW!�)������ �wR�Z�����K���vo��ԉ"I�I�iM}Z�O�o��Eg
�sѝ�54�jm���;#��2�DĴ�y})^�`oe����Q�d<#I9c���G��B����AЏ�+�/]�ӳ@oh��R�]��B��E��Y � ;���a�\��l�,�����q��I��t9ʎ���y{�2�jy��[p�]� ���fj�E���튥�s��{�$3�Q�֝$`���9�����p��s\���0G�Y ^�^�h9B�Ò:F�큹/��?΄i(���Q׺��x[r���ʙ����%.}i��7υ�EP?����g(w�4�`�U��6=�j�/����$�(�Q-|?����U�����5P��Q/L�9��Ań�T�jH���l�r��@�[`3Ҽ��:���y-��x��/��B�����]�tW%\엫QWl���Wd(4}I6Y�g�)~^�3�b2ثY�BֲE���7��G@��Kh^���2�_�| �~,���?ˀ�k��~���yW��Z�ײL�&�\�_�l�/��(�����W����g��G���=�u�`	Pqޤ�
�Ͼ�G�̔&����
�����fQ
ݔzV�g��X�n�N|�m� |��ϴs��L�y��W�2s��+�<4I��pRP_"�X�Ϝ�O�>A ��6��9�`�#2WpYf���,%�o*���o��� nKs_
endstreamendobj70 0 obj<</Contents 72 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 153 0 R/Resources 71 0 R/Rotate 0/Thumb 144 0 R/Type/Page>>endobj71 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F4 82 0 R/F7 106 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj72 0 obj<</Filter/FlateDecode/Length 420>>stream
h�|QMO�@��+8B"�~�]�f�֚&��Ũ��]ZЊ��݁��61� �;o޼ys:���q��sz%\�&���S�bxLH0G���#� +�n�M^���y�b��(��K����r��1ļ�|���Ζ�w��sr'�!�C�)�K.�?y�$�I2Al�� �ƿz�>὞A�BCo!덪���i�ʳjS˷���^w���q�����Ӟ0F��*{��n��d����o�ʆ�%���F��������B�cB}��&�R'p�	X��M�����T��bfzj�=׶W	���8���d�[�ٓ\�m�n���/UQ�rLɴ�bp�X(�Q8j:p�k�.WG�`��Y��s��/ ��%T��\��R������V����4��v�^F��Pu�8? �Ա
endstreamendobj73 0 obj<</Contents 75 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 153 0 R/Resources 74 0 R/Rotate 0/Thumb 145 0 R/Type/Page>>endobj74 0 obj<</ProcSet[/PDF]>>endobj75 0 obj<</Filter/FlateDecode/Length 8>>stream
h�    
endstreamendobj76 0 obj<</Contents 78 0 R/CropBox[0 0 612 792]/MediaBox[0 0 612 792]/Parent 153 0 R/Resources 77 0 R/Rotate 0/Thumb 146 0 R/Type/Page>>endobj77 0 obj<</ExtGState<</GS2 163 0 R>>/Font<</F1 160 0 R/F10 117 0 R/F2 169 0 R/F3 83 0 R/F4 82 0 R/F8 109 0 R>>/ProcSet[/PDF/Text]>>endobj78 0 obj<</Filter/FlateDecode/Length 1160>>stream
hޔVK��6z���Q.V\�%Q��6-Z���=�s�%��ĒI�c���rH=,�.�`��p���y��|d�]�x�^��N�t�~^���!�� �9�!Qa̖Rq"����r,w�N��œ�p<�|N	��*+��I�a�i�ׂ	��"��L�IȩԘ�z��֟��l�e�ֿ-��&M�|W7���%	��Y�Ǻ,�*C�1�C�V�.�x�\9����9���ɓ�Z�>W�k!,"����Gޢ��r�CҶ��,�
U�u��ee��'�>&=��$iZ7YQ�Lx��0��҇�#���y�Ɋ��
�P�pO��F�S^��xi��$+_0ȕ��obvMmf�t|�&��������W�}��rG�%����3IҒ����D�Ʊ�O�6V���[�<h֒���ka���JH�?P�L:k�Az&�(���ʗ��9O;���6m�cW7Ù&w⡀g7���֥w0�_�21L��CȽ\��S��L�Xo]�h�]3PWƏ��TnW�BX��Г
>r�}
CI ��H���`Zh)H��x��bd�!���a�ⱃyz���t{0Ј��٧I���H�t��f���͊����OD����`*օk�����bAe�]o�C�uy�Y݁*VDb��csc�+��
4��P4����-��g��h{�8u���l�7"�~�����Hۺ�3�!6;�l��w�&���+�{n�r����TN��+�ʩ�~��I��6Ok����߽������z"	A���[R��`�Uz8e=��wN~G8	�U2����� 6[���$��3�)F�!vuhY��+ԈUR�r�|��f!�Τ�3��b�.7�to�T2_�H@#��q�_G٢���r:k8T�J��筪��f����L ���y��	��n�E<��Kf�`/���x�V��zteX�r��C�&��%���PC.^k`���/��UO3�h>�fa��HU=̨�FO^阛��U[l�$�{H�椇��wĜb^�F��;]��������|�rd!�X�>�GH�2��e�/����SH�g<��I�9h]|(�"�/6L]{��C���,q��c���i&���2�j�p;og]v��}6��~�P"�Fa\G��z�  ��2i
endstreamendobj79 0 obj[/Indexed/DeviceRGB 255 80 0 R]endobj80 0 obj<</Filter[/ASCII85Decode/FlateDecode]/Length 428>>stream
8;X]O>EqN@%''O_@%e@?J;%+8(9e>X=MR6S?i^YgA3=].HDXF.R$lIL@"pJ+EP(%0
b]6ajmNZn*!='OQZeQ^Y*,=]?C.B+\Ulg9dhD*"iC[;*=3`oP1[!S^)?1)IZ4dup`
E1r!/,*0[*9.aFIR2&b-C#s<Xl5FH@[<=!#6V)uDBXnIr.F>oRZ7Dl%MLY\.?d>Mn
6%Q2oYfNRF$$+ON<+]RUJmC0I<jlL.oXisZ;SYU[/7#<&37rclQKqeJe#,UF7Rgb1
VNWFKf>nDZ4OTs0S!saG>GGKUlQ*Q?45:CI&4J'_2j<etJICj7e7nPMb=O6S7UOH<
PO7r\I.Hu&e0d&E<.')fERr/l+*W,)q^D*ai5<uuLX.7g/>$XKrcYp0n+Xl_nU*O(
l[$6Nn+Z_Nq0]s7hs]`XX1nZ8&94a\~>
endstreamendobj81 0 obj<</Filter/FlateDecode/Length 4565/Subtype/Type1C>>stream
hެXy|Se�>!MHe�-8=�{0�*(�*d�ji�ҕ.iӐni���IN�fk��m�to�ӝE�HY\@e���{gF�/��ʜ�up����/��_����>��=ߡAAc �6q�u��-ݲk����,�#4�SA��MAǏ�?=~zi|��3�zsk�0�I9	�'w>�T�h��buhz/+3�?cKz|�;�@:73cFN?aF|BFr"7!~Fl�ܘ��������L�	� M�A�h�4�A�!h.Z A�i�����C��,hK�3��QPDѦ΅ў�-��@����Л�m�h7�J�	h�j���Xh,�¡����=40�@�_�=`H�[���v�v�n'�~"�����0�i��	]k�|���I;&]��n�)�S�L5O[6m%{�����^:������<��h�	t�7�}�4,a���,�D����Qiq�I`��;���I\N��SRj+,DG^@�y�� ���%b�8����I0�kUq~�/+����@ �$�60��)�v��3����!'�(���b�U�jXl%Jщ���`g�����np/��$
�]n�)Tx���!��-,���f�ˆN����K������ϯd���t��j�C#�D��K�T�� �޽v��y��n+Xz&���lNt�'�^�M`WZ��6"��i�׬#��a��"��#T������T��uw��?�@�;ONdˬD	V�t�k_Ε�Ò���G!ui�N���K�3�h�r����S��o�x���W���M�_^�om:��7g��$7�����[ꓨ�j��U�^g��ɜtn��]z��{��қ*���ȕ�P���"}�e�Ӄ�``��_��$iYR�.O�z���5V���:��$�v��c*}���L[	Z�Q�Q����|���!��J��Aі6ckS3ވ�D�SA�qk��d2�:�Q�5J�
K%��r�:�L`RU���|��}4��}���j��@o.�:�L�&�r��5�K�ۋ#2��Y��|��� �Rze�Z�$8F��CN���b�I0k�<�����U���w�_��98,+$Lv��� �������>��H\F��'�Ϙ�m��<�P#ĴJ��%v���q���Ǡ�3�`����:<W����_BV3���j�7  ���7��\�gb:!#_�/(5��`�ʨ"�����<�+�e��B�#��|����
�b="�R��	#��al�9�w#6�L&c�NX�����Q .�nd����f�� �]n6v!N;Qj՘�Xj2Ή�2F#
1�#�.��t��tÑ[u"XCh�h�R�4-��K��,Z�� �n �����a�z�^�?���Þ�Z,��M�#��n����0���|�S�mt�S~����m�O!�VFh��<66��(%#t5�y���#�jg�(�H��ȕZ�� ]��xB��CA�l!,v=a��O+��d���S��f.��2K��ߥT��}�۱1�8D%�:`%ܾj*�Ո0j�i{lD1�����K��U�����a��(ѥ�h�E>���L%���Po���m��v������`��OA�Z�"����a�4fN��ȻC;���VKd ds8�?�4m�!ڙ �҆(��\��`u�F_}��C�R/�v�U.�H�P��C�<3�#�@(]��d��1� �zL�:��[[[����k%������e��������
����N�%6x�I-0�o ���s$#��R�����n<�֎��\�aT���f��h@n3/;�9'~�~,>)B��Sˤ��奪9�}�)�\@k�bj���b�Y��|_{�9%K~k9�J;���Ō�� bv�	CE�\���ӈ�3��7뾳Z�f�X����μ4N*��W�W+�U�����O&u�)E_,b�aR��YC��RO��s�M,Vi��ލq�"2�##ۛړOQ3u�>���.>w�X�g���!OK�S9)�TDM)Wj�ٰ>�5'�k�Z���3H6g�s���]�_zM�&/���#�ٕ%������A���3"K��o���:��T��^�b�Ѓ.��9�q�"�`�=y&G��֡�N1!���"r��uW��<GڰH{Kv������VG�.TJ�;d�Н"����A���B[j�m6G���]�>�麣>\ځ���9ڎtU��huj�
�8c�p��ƞ�tpr�y��h�`�ʨ�e:��S0$:�N K��^�R���.��a.�M�Wt���`� ��>�HXBr6'Ň&��)�+]��Z��,��U�j�mmlE�wpbx�8�f�/�^�	�Z�uS���bn�����*"V����
o����$�M�"��;��s0����L
�B�����5K5y�n~w���o�z���G��{�6���G@����Qn'�_>�:�w��e���U�S[W�B?���R�PX���N�U��d�`��P�ҁ�u��^�Z=q��8�y��9��{���Ll������R���l�R�7�B����<�\I�`�~4������O<^�&�Iu���O�%���u5��#c��2��j?ތX=Z�KS�2r�RYqeeYSgly�|>9=Y�f�c�"��х0_
�l�Ū�C���DD&0��X������
����������@���N������?+��'�E��	�d*��Vī�F� r,9������+�:�,{�$B����/.��Gĩ����Y��)w�M�ǵ�����{Wݨ�S3�u�.d�������퇱w*��!��V(0\���nn�,=�/�9o�C����:��q�؍h��L�Ԙ�f��kk�J�Y�e�g/#]���-��q�8L��˳��5��w�|S@TbB��Ay
���H��UTYXe��ʎ���~��]��IIi������Cn�����O���A:�2�*�����Ee5�Z�4��~�f���p;�髫���{�C�X���>�Py`q29_�1N��� m�U��G�vuf���`�T)W�)N�P?�(A#́�+b��5�����+R� ��v�����C8C�Q���o5�m-��t�%ME�ó�VjU���1�?7�x�_2�g��.��Gxw!�hmg�%��ؓ�Sr�rl�dhU:5K��e��ۂ��FbB~�o6����}�"��OC/)��QHXd����er]4����&�q�mZx�k�ׄ^.u�!UW*���~�<��:)kH�8?���3��}I-������߫��{�b�w�;zR��q��7��m�^�q��>��3�"M�0%�d;��U�^�v����r�f���ٜ0lwOT$2�|����@����V�*�xKNݷ�&���5��7|�s�,���B����ksy���>��$�����9�����%Q��bUBe:v:��h�v���v�-�1 �����eK=��5U�|j��A;ۨ��ۈ���aIR�C]�m��R_ѡ�M��7#���qi�;����Rk5ds�&����\<J5`�I0�T�ڏes4
����*J��=��6����_9/�Q�]�n�n��[5;k��=f":�[���Lϋ�ڟ�>X�
&��Y]kC�͉�N��s�zu�,�H�Vg���QSK�I�2�F��[�*f��x���lJ���2�rfޏ����P��Z��h����Q�J]�ݛ���GT�x *��:��Ԟ=�:I�����7a����*���d�:����!�n�~$J��p��ʋ��c*r�GS|�K��y�G��g��:����2�(;����p��~�;���*�ݥ�*�otj�.��;St�'�/�g7I|�L~f�آ4+ъ�FN6���, ��m诿S�j*6�~Sy��s���ϝ���[h�3���^p�z��v�&8z������d��?�wl���+%R C�͊�򑹻ɧ2��bv���PR�ΌːD�`�X�F�6k�0{U�gEyX��V邫��b>?;%�#�������QvDZ�|s�e���؋���$��諰wZ1{�k����+�>q1V��@S������=�wVo6<����2.l���i����Dɕ	k�G {E�&�^o�c>�������x���������*G?Սl�a�����x�v�b�}���R��~����6,��s�b�`���CI��fpZ�X.s����8��/'�G��)m�B>A�I֚xr&�]���h��o���Hi�$2�R�V"�� JŏY�}]��/���L��r������>q�o��7�� @�� ����L�d;��<�^���u��ܾ����	w��ֽhZ�2�ONc�f#XIl�#eG�m���Tb�,��i�.�֟|ݾ�Db^y&�]Dy���K���̵C�<J%�
I��#�<�(=F�۝9͟w�`nz�� �F�ϒ0<7oW������X))��͎|�]�-���efq��v�8�AQ!�z�
�Aof>���%�� ��$c�cQOh.D{8>����q^����vx�dl���� y��
endstreamendobj82 0 obj<</BaseFont/NPEHPP+CMR10/Encoding 92 0 R/FirstChar 2/FontDescriptor 88 0 R/LastChar 148/Subtype/Type1/ToUnicode 84 0 R/Type/Font/Widths[583 833 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 500 333 333 333 389 389 333 333 278 333 278 500 500 500 500 500 500 500 500 500 500 500 278 278 333 333 333 472 333 750 708 722 764 681 653 785 750 361 514 778 625 917 750 778 681 778 736 556 722 750 750 1028 750 750 333 278 333 278 333 333 333 500 556 444 556 444 306 500 556 278 306 528 278 833 556 500 556 528 392 394 389 556 528 722 528 528 444 333 333 333 333 333 333 333 333 333 333 500 333 333 333 333 333 333 333 500 500 278 278 333 333 556 556]>>endobj83 0 obj<</BaseFont/NPEHPO+CMTI10/Encoding 94 0 R/FirstChar 2/FontDescriptor 89 0 R/LastChar 147/Subtype/Type1/ToUnicode 87 0 R/Type/Font/Widths[882 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 307 358 358 511 511 358 358 511 358 358 358 358 358 358 358 358 358 358 358 358 358 743 358 716 358 358 653 774 358 386 358 358 627 897 358 358 678 358 729 358 358 743 358 358 358 358 358 358 358 358 358 358 358 511 460 460 511 460 307 460 511 307 358 358 256 818 562 511 511 358 422 409 332 537 460 358 464 486 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 358 307 358 358 562]>>endobj84 0 obj<</Filter/FlateDecode/Length 319>>stream
h�T�Mo�0���
;�JRġ�*�����!�i�(�C��l�:�@x����8b:�\7�x�=�m皀�p��k� U�tvZv�j{�AP��>N؟\;�։���q
wX���	�[h0t�
�K��E������&�PU�`�����G���]�A��t1����"h�*��t���d3��~�����Nʊ8��+J�R�/�Y;sv$���s�@\��J��K�K��W�֪�2V.��g��ړ�N��A��[3
6-0�qJ�ݕ2
\�̖��]ҥ������1Y{��>?�<[�j����~�<D��_ ���,
endstreamendobj85 0 obj<</Length 339/Subtype/Type1C>>stream
  NPEIAA+CMSY10 -���J_�n�T���� LNt�,�3�8��� CIcirclecopyrtComputer Modern fonts were designed by Donald E. KnuthCMSY10    � t ��� _���l���ó����C�����^�a�����[�c�����[�c�����]�a���O�>�s�I�Q�����L�O�o�s�I�Q�����L�O�o à�����O���3�"�62" �4�����u��?����Cy �	�
���3:33O��
endstreamendobj86 0 obj<</Filter/FlateDecode/Length 215>>stream
h�TP�j�0���tpb8���d���\�;���(ΐ���K�t�=��$���%A^ٛ#�,��6=���T`��{���t ���:G�Z<T���i8G^�p:>O ��";�p+?>�-!|��������Y���d��q�5 ��/wcoq� k������?�U���,P�iwg�*��`�m�w�����%���K�0 �hb
endstreamendobj87 0 obj<</Filter/FlateDecode/Length 339>>stream
h�T�?o�0�w>ō�:`1EBM)C��I�s�H�X�����:�~�g?|������oa�{\�\p���"�48�4t�]����c�!%��2/8�\?A]'�;-�K��j�߫;H_C�ap'X��O�g�qD������$�<���R��i��G�q�]�:�}k1��P+�@]�����%�˱�_mH�t���&�sr�J��E�bы�W��ZE��Xk����2��D|�I��	~�H�Z[6tq�!��s9Jo9�!.J�aX`K!��-��Ij�W���ኒ�&
%g.X��H|���[?�9jUl��bpx�C�����I~ Ս��
endstreamendobj88 0 obj<</Ascent 689/CapHeight 672/CharSet(/space/P/a/u/l/C/period/A/n/g/o/s/t/p/comma/W/i/d/f/S/w/r/e/hyphen/y/bracketleft/bracketright/c/m/h/O/v/D/B/j/J/M/b/T/k/ff/two/zero/five/endash/one/N/Y/F/quoteright/G/E/x/three/four/fi/seven/I/eight/nine/six/R/H/L/Q/U/X/q/colon/slash/V/question/parenleft/\parenright/quoteleft/semicolon/z/ffi/quotedblleft/quotedblright/K/dollar/fl)/Descent -204/Flags 6/FontBBox[-40 -250 1009 969]/FontFile3 91 0 R/FontName/NPEHPP+CMR10/ItalicAngle 0/StemH 31/StemV 69/Type/FontDescriptor/XHeight 428>>endobj89 0 obj<</Ascent 689/CapHeight 672/CharSet(/space/C/l/a/s/fi/e/u/t/h/o/r/G/i/d/A/M/p/n/v/y/b/hyphen/m/slash/c/x/ffi/U/R/L/g/f/I/F/P/quoteright/zero/three/comma)/Descent -203/Flags 68/FontBBox[-35 -250 1124 750]/FontFile3 81 0 R/FontName/NPEHPO+CMTI10/ItalicAngle -14.04/StemH 31/StemV 68/Type/FontDescriptor/XHeight 439>>endobj90 0 obj<</Ascent 0/CapHeight 712/CharSet(/space/circlecopyrt/bullet)/Descent 0/Flags 68/FontBBox[-29 -960 1116 775]/FontFile3 85 0 R/FontName/NPEIAA+CMSY10/ItalicAngle -14.035/StemH 40/StemV 85/Type/FontDescriptor>>endobj91 0 obj<</Filter/FlateDecode/Length 7891/Subtype/Type1C>>stream
hެzw|�ҶeYZ��	��]�kJB�� 	��{/�ݸ7�m��*+ɽI�\��m\(L��B�P�݄@ �Ys|����|7?����|���33�3������J �Y�b�+F�^�j�X��xZ��c����'�=�?w�g�d��>�N_��8�k8|����@0i�쐠Pi�w���/��`�����(�po/�;��^1sB����qX,���e%����YY�Z[��k�`e5�]��VV���>���IY�X�zYY	o�_[�f=��t���:Z���z���a�,�*�	�Zbn�����&\+,��ec�A�r����O��mG�ּ1ꍺ^�ze���ͨ����{����Nl�b��olߎ�>}�~U����y�=��Qd��ۋ��}����w��;����Ȣ�)�^J��|���e�1��)l��j��A�A��|u���#C.�:����߿�����B���`�3�c�F!|��$OD�v�	+	m��ff2Z���\�" u���lb"�RR������@/ֽ���n��d�{���x[���H��{7�7�mh��� �p����Y##���&1��N�vbB�S{�)[uz�:��*+���R.�Tm�#�º��Os.�B]wiO�a<w��8��J~;��w���E�m5
�&���		4m5�vj�=Xx�d���	$LZ0n,5�ѱ4���=�=����[����N���6��sx�����2�߭l�#���j���1!�@����Ɩ0���J���Y;`�~LA>��QY
�<i�|$Y�l@։H8�����F"�=qsQ.����b�r7���[AG^�A�5 b���_$�r�B��Te1�OÏ�	țB�}���g����+2s�|	��L;X��e���D�G>���h��C��:�r�Z��0�� �D�x�D�8?3;Gl)8X~L��B����1T���s��	n>�dMDEppDDppEDMMEEc�"�#�;B~Q��Q��T^r�,^���d�?�%JK��*J���W����c���w����}]Ȼ��۴o��f~t�R�N/�6ƃ��6�3jF����̧;/D��}c�a[{6��
����
ӏ1��w�8u���>���esLt]G��26w57}	5��/[̌޺*ȅ���|�y=�k!�u�'+8m$�x�.�$�ǹ�lW�L��u���f����g��8e^#k�]��O6��
�w�M$��gOD0��Q �-�C���_��ӕ��T�T���8�����P�1���O�|�qꚥ,Z��E2���$OyB\�D�ⴠTפ��)�9)m'�>�0�z�|�d�
6�<���M"AV���ת���B]�pd�ӗO�7Fט�ƚ�N�Vf�����n9,��Z��9nV��m�)�T:E��N�$]2YW��_k�?���6��[�f6�B���c�/�ݽ��ڒ�C�(3﮷��Ks�K~�6�B~L��d3�-��\+�z�S���-�LU^{둙���;��\f\��KLaЉ�͢@Lf)Š�����'ܻ��u���}[�+��Ixs�#d������zB��K���ﹻ�ʌT��S���Q��О�Y'V�QW�I\J��1\Ja�V����5�J09�d��>Z�����<��	s�g�:x<�U�t�<���TQk��Ku�xv$؀��A�b7�����6���{��H��]�JV�	o5�W��7����K`�Vc񶜦�{'���F$
c%ϑl������I�����}68F�*F�%A'��lL�m)u��ѝ�=�R3S��m�D9rc}��A�i��<���q�~�'�C�gd&#�bd�*(S��b�!2�u�9��m3�.s4x�eqS�@�o;���}�J�\���v�2��a�a�N��7w���$�G��?rT��/y �xg�y�b{	u���ά����q�c�J~ո�@uo�ѱ�>�9&F�����%6��bm%OVO\=��x�S�p���n�V&�����v�;Y��7ж:�-h5s��)g��~���wm�6���(�Z_t����y2�@	C�WƮ�neveה�#��qA�^y|$��@���_�_�)�ō�p�t�A�~ߏ ��-������d�3w:��i���qm�FfՎ�zzO�f�6? }�35y��	��!�1�v�Ό�Ϳ��z~��]B2���x&���J���lkT_�}I�ZN�a�4y��,[�'��NrϮ���/��G��-2=�.A�Z����d��V��%4Y-��hsy|yu��r�;�4+j�A���Z�.�-�h�{F�_��2�5��# ��&��d���/�j;g
���KȂ4UV��ǧ�ȳ뇘�[ת����|@��e���pS�N��Ѳ{�p��C�F��+���\(H��q;�{ԪV�C,�Ϫ����� B�EH*�.��M�.t��l�u8�`�#L��5	j�N����[����[�i�w��eg�|M���z���_��|ٮ
B�s��LD ?P����)�0�]������I�U�B%� ��Y0g���X�+�NJ͝��<��*�??��>A�`L�	X���d�<I͠!)S��^�6l�v��X��$n�E�x-�,��9���$��˒)��8S��a�(-G�L�f170�3w
�`n�G����d`�2���X�a*00V�*D�84�E���@v�������Y�vv犜��($�^	�Z��[vb F������۰��L:�ɗv��B�04&<r�A�=�ar��!�|�4\<Q8����b��jb�Y�8���/Sbl��{�,Vט`�I�_�Jv�y�7��@�h_�薻���5����z�7�s_����?���/L��H�j�XL�(��T&�!�̿ߝ�On��K��wN�҂f�<:��.baє��c&�>kTA��'�Η�%��d>�bB҆l�$9d�T�8��N�zw�����=4�F��'h<rG��1� �.|[y|;���N�a�����|'��l��]b*�b��LΛ�����ؗ%��H ^w�*��}��m���'���"�PP�k������u/�p�m!H-^��0��S�I���&+�+�&���L��+���+���S���Y%������,h��;��p򣍫���s�|���*B�!���3�/���¹�y�&��n��DK!w�G��v��'p:o6����K�c������O"a���>�~U3�L(��P�w�������d*.))%�A����МM�i�"�(39'!�
��,DWo!���DG�ݒ�+Ȃ���9���LOο��>����e��CO�i0~��+����n���<h�i�_���Z�}�����B�w� �x(��ѴWm�$B9���;M�>m]�O�ơC��*�o� ���(!##9���e��N��#�t�� �K܎M�����0�+^"����8uL*�B�N�^�xOj��)�B��9��OVk�i�y`��H�V��J�:��*{�w%DF�F�.�Gw=x71G�΢��2��GTqmO,��I�O�o�j0L��)��N��VvN���w}"��{$��Q�����C�1���9��y�F�pa�w�{r���Bȇ��~��A9��7�#�w��D��!
Β0M���꣟���cѳ?)���H	%$ޘ��&p�69q�
M�7Z��	�����G0��Z�<
7K~�����Z=�� ����{`�D�=-4�9"ؿ����Z�Q��Q.�9@!�7��)�1�52���c[�I�u�;����Fa�Zd��̩a$��F~t�~\�s�<Y�:%Iͮ����N���DX�q]�Fj�4���!5��?��R����|���7�mn���?/�S4�}�@>��|�2�#�@��ðἥ�w���V����/n�6׳;|���"γś�m9,�F_.�V���a�NNU�s�~1�������l·1g6խ)���|b V����G*����g�UJ.�c�]�7��y:F�;t�2|fD�]2�����\F��䟪V'�2~�D5A-�i$�4a�䃳.��y���sxd�_��-�������O��8{R��}똕��g����NI'p�1,r"b�Լ,�6/���tu���3��ix�۟����k��/��
O1��FA�	�oC�I�9�s
��h$��L,,��Qti%��e
NIa<7s�O�~���C�yҽ�y���햶�s���6��+�aT�?�Xأ���1L��pX���aq$<��N��	V1|�׳j[����}�0:=��L������ξ��ʹ��V����_6t2�:���Y�XU��KC��:�Z�Մ��a�T�R��3hq���׊䅜VK��L؀�w��ѭ��e�w![�����2�t�6�0��N���1Y�<B�|6y�tgn�-�����=����GSre����.�1� R��23�mǸ�n��-H`���Q���"�k&���Dj�:K��g/�������)J��
�BmV��)||ڕ���L�������$�������-���rR�Ӟ��γ[��H*I�J�1H��!֊T���t*�X�Scq�ov&p�F�����RCp�/Af4���⃜?�1m7�b�n<�ݔ��&����ѥg�10|`<Ji:��J�ƺ��5�f�֞e �X�%ü�/�Í�B�W�y��xz-ݓ�P:�E�[���d���ES�-����\N�AL黻1��y����;XJ|�-+f�� B���J"�ϕ�M,�%�r�k~>�Y �����td���4��~ۊE�y3�[����del�oT|�\Ψ���Vq��Y_�>����MLKO�M��z-�p�Y_ZUiy�������~M�L&�{�~=���9�p�+�e��8e'3ƚ!�Xyg�Ѿ���=�t��I'��ߐ��U�m��d�U�H�jbe�\�؝����5�62M^��)��c+-�h~Z�����ڗWK�;�5���-۠��\d;cs�zW���;�4��1�0�F����e�G��í�aJ&���2���1�O�����g��:��|�h�LH��N��v�7MvaU��dgdQ�Ưix󗟀Ɉ�H<��)&�աb<�2N�W�i:�]A�n�`+=t�<(��`&p���Z�ŷ�1��7p�R
;�Nf��{�&���a��x��7 ��I�Z�V��3JyPVU]�/������Q������H�����y���޹�I�2Y����1��}�uv�D�e��V~@w��N�مxey�0���ay"317%k+i��s�(���kv�\K�^�G I~�3��ךX0� w���d��c����V�#�kQm��'jp�v���[���ټXN�G�A�e�����/Im ���7L��'���bQ�j�Fgx���dm?���8d��H �n7�rS�i�*y���7�K�7znk�d��r��������F{�9����-����g�`K|�w�.]�Z�$*b©�⨪�"}E�Oݺ���� ���N��.�]d��$�Ԧ����8e���p�f�y�êc7ʁ+��>uf]�?�zL��a�1���JS[db�˫��[ܰ�Pu��p61%2ēF�^d:W^�gB�Z�e���g/5�5����2Jim�ֱ2E�,��-H,�-�*/I���r�{�3u��:)=f޼�]�ަ-l��?J�iX�m�b���yR��;��K������L����2:܍Ӥ��[̵Eg��ܳ�q��=.�	6�ĵs6�xQGΞ*kK�x��< �oȗ�Էk���*�vL�d�nL��/珜9��v�≿=�>�ۦfa5q�360%�����$��ޱ8����%i�`Ih+�yf����}��/#����9r\��W6ji+���M���t~Yn	�������&C-�S)��X�=�}��Ou�J��>�r��8.��EraLQ������ч��D��?r��3wIW瘿y�CzJ1��C�������M[CokV��_���8sXtiP��*:Vm9T}\k�kjԪ|�G��f���ڴL�~�Dn6s��Uk��.��.��]���c]+S�����Dˎ:ى$�����NA� �B�۹��3��B���D�����O,�-/�pN��ìO/���.����D�W�Et+~f�=��mi+��O�����⿤����������ď5
0n�.�����Þ�>��H$��G�2
�B��	���j�ʆ��N6PMܡ�]̞�6C3�k�ԭ��t�VzS��Z�+�"u�H��֙��h7�R����&;^���,�+���UD�h+���ݹ'��D�Ѻ��WZ8�!����*��ZXDB�^�����pr�>s'���ַ�ZtO��p/�pd�)J�J���o�$����J	�"��	�?Ô9x$z�{�؞k�YcduH�AD@L�4��Ou�g-�����s׿����u����׽L����:����g�	�b_eDR�����!���&K˩��R�F��KM����5n�|��[F�7���O���L�{!��Wr�`iT\��Mm��6���w�L�R���bO{�{O��h����Ӗ�ٺ�ڭ6�@���W��%���^��ٶ�K����+[n�uƸ@-����=K���;d��KL`���d�Ҷ�ۃ�J#;����C��lmX���x۞���4�C�&��9n�$�f�CIbFC`I�X=%�a��V�!y�j��~b����g���k��A�>��%̟��E.,}����W�q�c��@�aL��Ϛ�h;��w/�r�S�g�Hb7F�U}��Q�G�͍���M2��dgG�����#���X�~���h
OF
�>�F6P+�X��&�Բ�VEҡ�m>~:]X�b%Q|�6��.H��M��X,�|��;�&K���-J��R:�E���W4��1���S�%0cQ�plP���{DU��ޠ����{6�r�y^PV�D���hЋ��4���x��u�e�ܟ����=�U��K��i3�b�34<F�"Yz�G�n�_h� ����m;v쫨47��u{d�p�i5	�b�*;!��KJH�3K�M�Y��w�L��(Go���aUq�'�6>�Q/oc"�i9�rZ5`��
���i+͚J��U���]+եQƚ��_15^ٲ���ٲ�89���5Բ�i���&�j��f��&��6�����������>]ٮ)�_f6�I��b'����p*I�JKd>@j$�H��JO�*��>Ǿ���2���#�i	���k�!��X�K�Ysz�����.���&����?�r�h�3�r&��Q�j�S&!O��9y��p1� ���"��qfY#X� �ڮ>�d. *>{��%���G��-����b;�6
8^!��M�R��,���=�j���Y�tܿ%��X��0�^`+���m�ߍ�~�W�p�V�1���3B���v=�b��{�}���%��^z�Z�{6��Z����9&N�B�5�UɌ2�KN������wq��l,�����pG�'+��E�q��>Y���,�j����%�q�"�Ûͧ�5$E��<$��L(+//�v|M��1��H��'���IP�e�.�vH���k�#|�"�U�Z�U��������G�k&�?���7{�Ω⒫�dE\R���D��1Mu�w��g=����_�S ���)��b��U��%�Vyn�a�#�������l|������ѭ���i!U��*9"���Ǖ�u���t#�*��<1��'��+���0�:��1�w���}xef򢨨D��	0 �è�
endstreamendobj92 0 obj<</Differences[2/ff/ffi 36/dollar 40/parenleft/parenright 44/comma/hyphen/period/slash/zero/one/two/three/four/five/six/seven/eight/nine/colon/semicolon 63/question 65/A/B/C/D/E/F/G/H/I/J/K/L/M/N/O/P/Q/R/S/T/U/V/W/X/Y 91/bracketleft 93/bracketright 97/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z 133/endash 141/quotedblleft/quotedblright/quoteleft/quoteright 147/fi/fl]/Type/Encoding>>endobj93 0 obj<</BaseFont/NPEIAA+CMSY10/Encoding 95 0 R/FirstChar 2/FontDescriptor 90 0 R/LastChar 128/Subtype/Type1/ToUnicode 86 0 R/Type/Font/Widths[1000 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 333 500]>>endobj94 0 obj<</Differences[2/ffi 44/comma/hyphen 47/slash/zero 51/three 65/A 67/C 70/F/G 73/I 76/L/M 80/P 82/R 85/U 97/a/b/c/d/e/f/g/h/i 108/l/m/n/o/p 114/r/s/t/u/v 120/x/y 144/quoteright 147/fi]/Type/Encoding>>endobj95 0 obj<</Differences[2/circlecopyrt 128/bullet]/Type/Encoding>>endobj96 0 obj<</Differences[44/comma 46/period 49/one/two/three/four/five/six/seven 57/nine/colon 65/A/B/C/D/E/F 72/H/I 76/L 80/P/Q 84/T 97/a/b/c/d/e/f/g/h/i 108/l/m/n/o/p 114/r/s/t/u 121/y]/Type/Encoding>>endobj97 0 obj<</BaseFont/NPEIAB+CMBX10/Encoding 96 0 R/FirstChar 44/FontDescriptor 99 0 R/LastChar 121/Subtype/Type1/ToUnicode 98 0 R/Type/Font/Widths[319 383 319 383 383 575 575 575 575 575 575 575 383 575 319 383 383 383 383 383 383 869 818 831 882 756 724 383 900 436 383 383 692 383 383 383 786 864 383 383 800 383 383 383 383 383 383 383 383 383 383 383 383 559 639 511 639 527 351 575 639 319 383 383 319 958 639 575 639 383 474 454 447 639 383 383 383 607]>>endobj98 0 obj<</Filter/FlateDecode/Length 309>>stream
h�TQ�n�0��St��L^������"Ӯ�Zdg�ߗ��,�N�#}��桱���O�t�m=����3���$�v0�z
��I������n���.��_a�Tޫ;���E?�6��㓈�Źo�.�����N�óv/zD�\�ǝ�!�dm<�8;m�k�#T�����k@���E�8w�K{�J���0F�H8�|�|�L���i佺�~=c�*KH�m��0\��H�ٞy{�"�1�	SSE�HbgEDɸ���K��*<�6e����q$΁���xOi��r����#��qz�� j��
endstreamendobj99 0 obj<</Ascent 689/CapHeight 670/CharSet(/space/one/I/n/t/r/o/d/u/c/i/two/D/m/e/P/l/g/three/T/a/A/s/f/seven/four/H/h/y/five/B/E/colon/L/comma/Q/period/six/F/b/p/nine/C)/Descent -199/Flags 262150/FontBBox[-56 -250 1164 750]/FontFile3 100 0 R/FontName/NPEIAB+CMBX10/ItalicAngle 0/StemH 47/StemV 114/Type/FontDescriptor/XHeight 441>>endobj100 0 obj<</Filter/FlateDecode/Length 3882/Subtype/Type1C>>stream
hތXyTW����V1Z1�2Uhpר�$�(h�WDm"���Ҋ�bcw��Q�AT��
.����3&.��,*N���81��G¼��d̜�o�h��~u���~�4eӋ�i�~�2�wg������s�x�/.2O�m���/��ɃmL���רEKfف��9�����ĿD���)��o���8��?4����0ǝ����~�a[6��9�j��l�st�0("<�hM�QT?�z���45TI���15��&S���ZlC�TP��������-z�r>��ZF��|T�lBZ���F4K�S[�*Z���{��"D񝍋�Ae%R�cf39���Mf�S����;���>S��k�w���ٶw�^���;����~�x� ������þ��̐�G!̆��-����\�xDjU�D�^��ˊK?��m����r�mޛ����(�MN�����
���5������fs�`�$Y'�(�a/�U�ϝ
�e��оxaŌa(�ߩ)`r�L��b�!,j��OQ���O�vd���`H�D��a��B>�����_�g�J;��3#՜nh�T��%��ׯ�!�n��	˚�K*j�N��ɉ> ��β���r���6W�^�G�v���l0��ƀ��Sx����Dc�_r@��4���5�֋�G�ԩ�7�����R�`����t/�^x��m�g3/�<�N_��w����#��:k�I8f>�^���pk�z]'��D�*�h�ө��(��@�%ٍ0�0
y9�羓F�o1G)��Fc�^����Ru��6������қEi$��=_Ĩׇ0�Y���
C0YN��e�xp��G�Y{�F�?V�X���gz�g����F��߃bw���բ���I�ӄ����:�M|�9c۰E)�|DK��:���a�k◼y�Mp�I��=���c� }�F��O,��PZC7� �0X����ÂT�j��ϫ҇���;����Ԕ�V_�8�oM�)�2ީsfJ7®� >����t�E�D3}ğ�En͎��qw������R�4��	�v�fP(�BVC��(��2�P�Y���p�r`�y��a�Ti� �����HR�f�B�<Lg�Q���(l3��E�v��H)Mlsrs�$d\.l7ש�X���Z�xM!��̹�/��"������k7'l���?Ԩ�
?���?��
P��0�+ȸ� ��.oc5����Rڕj+|�������?�{�����[ԋB�O�cJ��*����:���o7ռ�"�$�
h9�sW����'h������2Z�O��GjSusR9_s��%�0	4%g�L�I�SB!s;)Gk�������{{~+���r%�bH<����k�@�D�3�[� ��\W�4�ou���ɗq�������f��x���a �rdP���y�X�x����[4�[ !�*����U�Ex fE�a2�J�$����-�δ�	\>�����w�)O.��7��Ix�}��oH����~��3튉G��"V��vK���SJ>\#j���~O}6��t'�4ԃ�x@��l2󥦤R�$p�1�X���;X��x簰F�8[a	^'v����N� '��/��ZD�g���jL��� �[?q��x~7y*!�,A���0YRtn��D:��^���`ncfz�%��0��G
� F30�����t�_hP]_��:_�j��[�w�E��G|��9�����I��|��-��$�0�y�j-�#	U<�>LB�Cq[#A����5�Q��@I����G�d�D���d�����>"����It
j���/�}ww��y�(�[2$����{���/C�2ļ�j⛈y1g�9ջl��;�p잰�����H�x�k#��:~�ӟ���X��?��u����z��Cn�~T�Bo���5~[���)�" ��H4s�\4�"�-�R���{����{�XK���Y�LM���`�pZ��p�C������ c'�.�4��
Xy�`�}�G3��[:��Fb�E�b�.>|;j�qz%����a�Mu-z��'��ӷ7�7w��,V�O��L����*#6���{zmY�3�&%���bC�'��W���>z<��XU���Zo~@�VZ�;�p�Lf"��h1(32o�0����x!���o'����1��,���f2�}�ݭS�,�]c,t���z��i�c�Gx�
l��Fs�������\}��z��_7����s�8-W���?��ꋄm��B3S��J�cP��Ą�1�ƍ��|(x��I�*�,�@,o��)0�lZ�8얠԰Ґ������������[Vw�0��=R�W�|�����&�zr����x�G��M�V����A^����_��8�%ݫ��j�~�t5Wr�A�<���6Dƫw�R��y�1�U��oG����2P�ڃ�FCL��(d}�'�E�Mg�}8s�^P���}
��ӕ�^�֬]����NC'���oF�ˇ�%G��u,,LUJ��}�F�"KI�]N�j&���]iF��5b�q)��?WTd�?0ZW�*�mk�B���A\Q&:Z�����hy�#ϝǓ&������0�hx���ww����+�x�^=%V�$՘�+U�����y蝄�H�_�}#��=�`��&�Au@?D�C8`%l���^72��� C����6G��U��on�>S>X)8��\�O�5��ܘ��W_Bz�(M"k���臟X ��>Kn�Ȼ��Wa>�t�p��am7�H���9bYNaR�IeH4��ԡ��������]f�h�>`����)���a�����V���������j��k�"�g�,Ѐ���+e	�W	�V.E��/dJ7��V�3NM�>���.76��w��6���sx#WJ�x��j�2�]��t�":$ړ1T�T��/CV52:������y��r����?��I+��'U�5��1Qr��-�
#g
��/��o����:Ȯ*�#��\���bRo�:!�Exi�����`�TޒTϗ�2	�e��䎶���0p=	V4'm2��%@\���3�>�>�v�L�>�6~�Z����q�����l�O<��g�����UE;g�7���9k�����y��B��-���xG"�Y'��L��=�S�������3���<�~Nǋ8< u��ʭ�ϯ/l���w��[Ъ��ISx�`�=$b�j�нz�̹�&��D\#�Q����a�<�i���y���.�ɺ�\��t�PW{:���|v	~	q[�`��nc"S��hǸ�ʞF"���`O���1^9�?F�vw�ڷb�����gV_�i�Rk�b��/��,ww�9�D�
�����B~Y�޳-�7�(?�`H@�&~;��M;
v��պ���,��1�Գơ�!!W�
��>��EL�Q(>/-	.LE������Ө�/���v���~�S�T��� KR_���W�J�M��B��[����n	f�O�}$l����D.�����E���>�:މ��F㹫��';/0,x9�T�sS��r薗<���[��3�GZ�����j�x+���O��&a:n�}�vU��k,�]�BN�m�Ci�i�x���ʇnH2E����F�׸?�H�i� (V�J�a���&ϐ-�K����W���7��c0%'���Օa%[����<%��}p��Q�ۏ��Y��Y8562�+�
�h)�۶�����i�����3����zø��3�0 3~��
endstreamendobj101 0 obj<</Filter/FlateDecode/Length 5966/Subtype/Type1C>>stream
hތX	X��N���8.3����[�V��Vq-(��*��Ⱦ�@X� Ʉ%����5,�Xw�����뭭vQ\Zm]j�����'��^����'�g����=�yO�<{;��wX�|����{x�?���8�����Q�e��������[�,y�t���aِ�QT�ώ�������2?�h��-~Ұ�[��dQ��~R��[���������� <�'d�腓G/	�������� �"�x>o�=o��7ێ7��y�/���	&:���گ���!�|#���9γ���e�.�}�e�#�vC��C�����.\"l�fc2��GtM<G��7�_|�?���������.h�:��"�ZG�c�㏃���_2k�f����*I���C����a���������9�a�����g	/�D����������?��v� ~fU�7[[}��"�"�F*?O�M�2��ج��7�i)	D�^YZ��(�Y��Q>�X��j�,=7��B�� #��x�h �R�E��eDA�!2mB#P!p����Լ��VՐuy�����<�38�p'�ue���(R���)���h���eh�1n+�9�R��e(�4���6�(��Y���8+`�<�8�$���e6s��f�i�l 
��AALPj��5;�B�s�B��I'�q��"UI:�刷\������L�n
_�#5�aD*�};���A󥺎2n5\�'�g3����L�T���R��푥bD�?�C�y���
w���LqJV\��P��뛟� ;�/6n���5H�������\䷳S�,���[��� &�F��� & ��)��]/"��xkh]��4$dkcd�ڦ&�a����?�N�����r-S@T(���tE��B���H���a҈��xCU~Y��rH6�f�|��4N�lGv�~"�Z�;��Q( ��q�c-�]�r�]4���̹gf�O�ZdpU=�^�˙��XURR�7�39V-N�Ĩ�	iEFY�ݮ�T��QBZ˔ѐ���2�MQL=�Z�2�uB�E��i<5M��$Bj�r
�a����:Q%S�ĆS(;s���}���������c�!���[M�zc��T�Q��O,;�t���͆i׭p����!�,x���w~�	���CV �����S'���B"4p��EA�J��N�j[V��t�)$�>�������#�j�xe��	#$OCj8?b��tŲUK�l���P$����35��,Z<��
p��?޻��k#�<3ۏ���v�.��S��*Lo� ��1[�>���x�mW�3l{zA �Eeh�4��K�����5�&3L7;�\c+4�#��Ѿ4��t�KK�
JO�Q�(��5W��m+����T�&%�n���т�>_Rp2�� �@4��c��[f���a�ow@���_Q+Ơ�f����NO����p�7J�y��Q���Ggdd�∨
y]�A���b<?Bޔ�J�y���` `�:���g��7���mK�YN�u��;B[(����D��#��q�S��+�s>�V3���A�!����(�J&�E�/j��&�<���";�������\Ǥ��Lt,�#�e�w�0��Ͱ�C�������2���h?�~����{�t�8��6���@�)���h ��l��~II�΍Y�|.1��4N�n?��v��H�F��L�v������N-]��u��n���V.��NH���m������ߙ@��k�c�ܼ<����3DJ���`(G����u\A���H0v&�X��T�;vL�� ���ŝ�|u�֥���L2y9]T��J�Qgd�&��WΤ���6
b�_� t��U^R�����{��o�����l��^D~�6	р��W���$�h���O�bp@#a��N5���,&%��K$o<��Mx�񓧦)˪�%���s��O�=������MVV����z6H ��+U�!��T�2
�2F���U��D"���SO����?]��f`�o	��\��0�L_�4�dlKʣsTь�D��(�K�����Ę�|r�
�z+�?��C��܃|�]��oc�:?_��Xqe�M$�P��dB��F�w)1ɳ,A�XǔҠ�@x��e��J��H1@a?6��^�e�t/�������e޸���J�ɨ�bS���$}7�/�?�O�w�G1ɩ2&LM����KN����wo��y���R:>';��la��n�W��7-Pa�e�u����RFc/����a4L���x4�F��C8�| a;�Ĩ�
�V�m��f��n9`�H�|h�Cm�X���;{�>FN-B���a�ȗIh��TUR�{�N𥸉��k��^5�+�&ն@������j���TA���FqG`�p����(����b�t{�2�O&�h��v�43�7�wY��#�����T3M����F0����^�'��ZC:�� >&}��z�9����l]R]O)�Y�����6���h�ۈD�h��\�8����@G3�PzK�\�G���ae���O�e�𺔈��zW��x�0�c�R',�b��j��k_D�j{�f�dlƬ�ش�L]����W�r�-���Q�w_:ŷ���kg�+�q���k�Rn�]�_@�����c8H�D֊�eᨆ�j����I��a�0�K94��:qM��Q��znd��.�(8���͞��0�4�m8�<rd%~y��4]qz1YRZPZ@��~]�q��+{��JQ��ˋ�d曩�L}�>��!�sn%{ߞ�ȉ ۊ_ۥ�ڮ��!Hʆk1Xڕ%()�ԑ��9�hVWk�o��S���zY���\���no�	�#����5F�;,������d�4��H�t��u��`�.:H�)i*�I^kI�(������wfA�jjUrd�TҚ��f������+�3��<�+w��w��D4��i�.e����P�U���=طh͚M�&p�p���mN�[p��XIL� r����u�+���1�ƿl�4�!~�s��ha�$p��I�}r	A���u���	o��b�h	��B���乚/w5s��"cB5t�zk|8s��9� ��Jj��E��:� ��.H�C��&����M�g��˭�L��g��T��ME0i%.�%0��*]is!�ۜp���۝���&�о�#�?�K;W}��ZG�_��e�BtTtN�����#�{?*#>4�j�]]�M"���3>=��}��y(+�����#-0�2tfOۏx��XPZZ=O���0�K�)z	Dp�7.���5$[���H �Ss�36���s�۱Q�s�������C���{���=p�Ҍ�3��U���i�}fL�#F����I\;'}�H�ђ�Omώ��ʝ;�\u�~�ʙ���i-�9��Q�3����p�3�z��觴e鞚p��y/X�щ��A�J�ș�:+�ye�s̒��L?Ay�����4Ͷ�jb
i-v�� ȫa��Bc�F�䟀���b-��}�i�bi~�O�0-y��L��8Z�r���� ܦ�]{4�_KD!�����C�����a����mi�hR���K�M�Q���_��)y�U��L_Q��-K��8%��'���w����/�?���>׮=��);3�o~dv:i��	�Ik׌�T��"MU=�瓒��FC�����ۛ�0�B�	5e��V-U���)mi	�L��X�Zr-�B�k'�ι=i��o:D%�2�C����G��{sZ��w�m?��n� fvK�E� gvn=��gR�t����桡�����ٗ�L��/ ǩ����;.$�Fk���Μ>~���yLX���ۇ�"�1�X �S�n>n
cT	)jy:����r!g̿x��	D@�`R� m����1����x����Q����ǀ�;~�����n�w����ܟQ��&%���W�@S1I[C�o�&�'LD4���?���7�h�_ Z�=��]�|p����m�<��u���'/C�%;V�F� [�+�L��1��c�c�Cwj��ťeŦӟG�N'��=�S�=�_)=s�"y�C�va�v�0�:K-o	@�b88��!��WL#�Y���P]n:����'>�֌u�c��Ǩcl�Qcc1w\N�<�:5�������+.K��JX���9t�����N��Z�r�FO�� \��K�F޻r��1�!�D�krK��]u�6R_]�P۴:y�&p]�P����`�M���XZ�x�	�ѡD@��0�r�f'ӟ-㖨��:~l�;�y=���S�>�KKd��h��)M��K5�l"7�4w�9|��}�+
��tym!s��!3}��M㺳�]v����:|��o��ށ��,{n���ӰRk��\�7�B���]G���F!�����[���.��R79�ߣEnZu�Do���'�Bԁ��чm�x�<f����������h����ۂQ��ck���Nn5���i|*���0�k��EP~!��K��V��&;�0��1����9�2{`�Nx����������8	�
pn�	[����}T#�f��Bx֓S7��u)��!�>R}�컫\��Y��}�o3��]I��P�+4���x��}ZbXo1<�5�X_CGIn ��7��Ll^�C��[��=>�pK),I�:��S]�F!�(�Eٯ����zK���꛸%�ڡ�������(�����|�Q�P��)��w��?p)9��@&(0�{9��j����NMN���X�'��� �/��~y N�A�q�k��^�p��y�)����Hj������b��T����X��@����R�����?���8�j��]͹����{��x?�!·n%���ts�����냭�F�룭>�>l �rݬY.n�ܻh��ځO\���IXtKr ��L���DEi��Z��P^oH�;��%qK=h�eG⑝�7P�sȗ�i֓{��fH2��%��(�Z�\y�4O���������KC#s�xmv|S����Le��2n��Ē�#�,�����SҒ�mi�%�!�n�⵪躔���,��Q�㻅pa�����o;�6�0���ÿ#��g��\��ր�r�&����}�h�M����>�M��(�����7�uV0ƀ ML<�Y�"#?a����ݎȎ�?��`��� �N]�e_�����wN
��lo���U�F��Kԕ={��Z)��cp�̯a#�<���>������!>���pMh��/I��/,�v]����^D;d��3��M����7�������W3�t�j�̺ăI{��Ⲋ�N�'s�u3�2�z���¶�_�dL���L#}u
e�9����Bs]{[[�Sũ�<�V�/7M_\�i�֜��^��׳�Vb���;��}��ި��z�e��º^���Md/=�Ơ?zFӅ�m�B��u�8v"Gax�v�g;c3�;���n����G��[Uq�!��2՘^9�GԾ�M���sS��PKg%J�0�B#����q҈��H����E��-�o����{��F�6GO�fb)t�g��`۸� [�.�6a�\�OV1��U�Cq��ӗ�q��X_/���ev�P�*ng��:���� n!�'N��5*fm8�ȟQ�'P
������Bk4��:�W���(��>F���L?Lrhhj�jSi4*���D�tQm��Fjom�vm����&�gU�~in ϵDde&W7J��h�hsY�>�*X�I�E�0��QSV?Bj��_7�h��9b"�v2e;˩����5��4FiS�(-��V���$ȿO�+�O�\Q����P�Sᡑ	J2%��8�.I�Ɉ��x-W�E�	e	�E�<]>ej�+/%��rwb�!fYo*�Wӆ���
��!���"��Cq�"ʸx[i���g�o�V�_ o��
endstreamendobj102 0 obj<</Differences[40/parenleft/parenright/asterisk 44/comma/hyphen/period/slash/zero/one/two/three/four/five/six/seven/eight/nine/colon 64/at/A/B/C/D/E/F/G 73/I/J 76/L/M/N/O/P 82/R/S/T/U/V/W 91/bracketleft/backslash/bracketright 97/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z/braceleft 125/braceright/asciitilde 144/quoteright]/Type/Encoding>>endobj103 0 obj<</Ascent 607/CapHeight 607/CharSet(/space/n/a/t/b/i/backslash/c/e/g/o/r/y/m/f/period/l/s/p/hyphen/u/d/x/bracketleft/bracketright/braceleft/braceright/k/h/w/L/nine/one/zero/comma/P/D/I/quoteright/five/J/two/C/U/S/A/three/six/slash/M/R/F/T/O/j/N/v/E/z/at/W/eight/seven/G/asterisk/q/B/parenlef\t/four/parenright/colon/asciitilde/V)/Descent -227/Flags 4/FontBBox[-4 -235 731 800]/FontFile3 101 0 R/FontName/NPEIBC+CMTT10/ItalicAngle 0/StemH 61/StemV 69/Type/FontDescriptor/XHeight 428>>endobj104 0 obj<</Filter/FlateDecode/Length 266>>stream
h�T�Mo� ����9n��j�&��m�H����hI*ă����6=@f�y���6�ck���&١�^�p�'.8hyJK�{閣�@Cr�����0F�G�ޭpx~��9�N����+�n��G42���6/¾��ƴ?v^-B��|/<)����X�Xu�F��]3.���l�,+�8	ڝVW͖y�6ae�)�2fh����@��A�W�ޏM���敋sai�i�8�6x�;�8Z<�W� �j��
endstreamendobj105 0 obj<</Filter/FlateDecode/Length 295>>stream
h�T��n�0E�|�,SuaC�$Ģ�*eчJڽ��T�eȂ��MRua���\?���t<�~��G]�mo��i�x��`�[H30���U���2��4�p��e��*N�_`󼿗w ޼A��6���@}q��3H�*0�&��ܫ���yqYX������)�^������� ��_�:�V+���2�iQ%Ի���-q�L�$%i���
���6�4��A� M�-;[v�h!M�a`"x"�Λ H'e��ȏ������-3}��_R�z��_t��xx$� �ޑ�
endstreamendobj106 0 obj<</BaseFont/NPEIAC+CMSL10/Encoding 110 0 R/FirstChar 46/FontDescriptor 108 0 R/LastChar 89/Subtype/Type1/ToUnicode 104 0 R/Type/Font/Widths[278 333 333 500 500 500 500 500 500 500 500 333 333 333 333 333 333 333 333 750 708 722 764 681 653 785 750 361 333 333 625 917 750 778 681 333 736 556 722 750 333 333 750 750]>>endobj107 0 obj<</Filter/FlateDecode/Length 3565/Subtype/Type1C>>stream
hޤXiXW�����UDcl�*�
�q�b�%Ѡ��TܷA�l����&���tWխn�U�fiD�5NF��q�&.�1jt\nu.�j@c�o��2�����Su�s�=��m��w/�T*uY����s='x.]�d���ɻ)���~��t�%�p�|��g��&��%�|�C����p�[�#ޚ<H�K*�6�32<J��q]���u��	r�����:?2bsX��G�zE��!=5%R� �d��d�\2��d�L�T|&uzW2�i�|I��r��K�$c%z�/Ҝ^�^�d�d��C{�bs���j��S�|Bi��>W���{�_������sqs9>�h����%z�Z���|pjx.��n:���)��
W+kw٪wS.�&4���I�ïe�h��
c0��PS^�'�0�M���ty�P}���Y����ԉd��L�@�r04o1�9�p|V��j)�]�e���c�y?�+b�<E���R�����'L ���pğ\hm�?x��;�&��Fx�A��y���M;�#38�����7�$/կZ8o���i��`����Bp�]T5_A�S���t��?�����𮢬DT�%���m�؍����Y�g������F�"�2$�'}��_�} )(�س�ڧ�RХ���ę�O�Ao�}oyB~l}Ki}վ �J�8t5�����*6}Q�Z:h}��$p�`�c� ҳA��kH؄�e�τ�2Aiw�!;O2�8.%��t���4iLr2Kp�R�!����g,'��n���w�1�쥍�|�1W��/SgY3����p��k�>���}0�:����%V8�F^���wC��
u�N���η���x�e�ACǯ�������|i��,�e�),��X`D�)#/�7Z��ֆ�;��m�k�S��)zc�r���i4k����ޢ��2�u�
��<�����`��r8�����}��">a�����UқW�IĶ~��;���߾�7�9x���3l)�y�dD�2<�4�j>ڿXU�
�FK�::���O)�����3ŗN���S��g���:��*6Vo����Z�_$/~�LE��鴌�93IU��~9�L
	�W&|%<Wd[������ �����"�
��.��7�M{@-	��O6���Y1w�2������)�DbA\́�k&U�p��~O.��>�J�~��(��/�xG��F~��&ܿY� ��G���MѥTz��X�-�S
eǟ����d��������Edy`MB�<�*���8}᳛}W,^��s��JEE��͕I�:�Fb����\u����Wn�P.%�7؄�e�:���g_�(e���d���]K���?`�U!ы^ٞ/�kl!�α\�U�iVd�Z����.k�<\T#� ߉�a<�kb���8-��i䦆/�r_Ƕ�|��vܓ�ݝA#/!Kq]��
wy��ޚ�"�sV�l�
���x�l�(��]�+��L�"t���RS:��XF��$��xR|I q8J�?�3����q��w��g�\(��:�Ј�2�#���En�`�;�C���"2��D��`��O�'��sC�i֋�wT6�D1�$�dv��c�E��a�b9Fϲk��jǑ�g���|W�y��mF�P"�Ñ3|����w��?�\�g�@�R���s�{��{�LZGNBΟ�Ņ �����>q_D{I���rZ��[@i� S��LB�6�]j��߻/����$8\@/����;��.Tt� ��j�d@�f��W�{��39��$��v�t�N�`�ni�}�����@Gs�n\�&�h�����-��Z��x&�I�������'�D�M%��-P�ՙ�YI��]��4$rj����:O9ϊ����1�e5���]�6=�� Ux�:��5�-��`��x��W7��Z��D&�I|]�=9>ڍ���B˫�r���od*H��r�������fr�m����݆�T��x/%S���)6at�t�C_EC���N������m���#ɸ-�G�K���:�$��!J�Ǥ�W(�x�Q��pa�s����oN����5�tJ�Av�.�`%�%P�S�QK�a-a��I�>AP<Zh �q���*�. ��+2�]�p�w�D����~b�ՙ3�VG;aB� 2����=�.�����DF�DS�������#Q����� ��**��%�YV��[����Gq?T�_|$N>�1�6����5ᙸ-c�M���Y ��L!�V�_����۪�H�A�vkt�x�7�%�gs�\��wdv�������O�|�w�zK?�u�܀?8�w��J�%��%l����~^.�O�cz��"���,3���bZ�ѯ#�ᰱ���a9�e�b�O��qW��x���bj�1=���ʱ�i���">������x�A��i�w��j�7T�f��F���'1������;��$��+�;�b�G�w�b1������Ph֙�M���x�M,%�gs�|�\b��	Ǳ�LK�?I���J�.3�atFV����l��w~���N<5BUm�^%}�X�~(�W
�p!��#w�V�?%���^�
}�z�&R�T�h��;�mw6��%��/�m8M�x��yĢ�����^3���3tO5<#����XZ�+�p�}����I�����7��ƞd��v*jV
�HO�["����z��RYE0�Z�1� A�#x��RP�{������w�u�jw�O8�tH�K��718����	�p��>m	��j��]��q��I�ِ�M�tj�Z�J�1{��+��J�T�4P	U!e�����1�#���-�n̩e�� ��0�U���=y���-۽F
���!�������|Wbuq��x����(�Oe���Gۂ�3��]�v6:sq1s��;�z�TAfI�c4g�8}C�"߮�G`�%��s-T�ILi���C��\�2�hP�g��{GP�x���Q��0l�Q�r��[Pvx�H*���N�m���MT��!��P����2!SL��mʍ95���}��D��L�آJ-
�+�JF�۾�f8��Q�,��53Ʉ�����gĤ�$�gd����y���� S��v����<������oe���e�k��A>\(��2����
�C�b�l�2������sa�����NcAj�?�.^�K ��ԫ���ʄ`a�΂�ʊ[��e�-�E� S m�����T���%��M:�F�Y����.�DaM�>��\O1�����@�ӈx��"G�-�!��P�:?/����il�� `Z���E��܂"J����JŲԀ刴�_��Y��pQ���3��ꢖ�{'g�&ӯv��2�:ᆢ���f�lƆpQd�
#�
��R4i:�������`扚C��k7!sEvÖ��`��W��;h�����&'�gՌ*A/׫��V�52�i�Q�YN���ZQ�]����V��F���?߉*�L�H_:�����Ϛ��,0��Q��` R�I-
endstreamendobj108 0 obj<</Ascent 689/CapHeight 672/CharSet(/space/C/O/N/T/E/S/H/A/P/R/one/period/I/D/U/two/M/L/G/five/X/three/F/four/B/eight/Y/six/seven)/Descent -204/Flags 70/FontBBox[-200 -250 1123 969]/FontFile3 107 0 R/FontName/NPEIAC+CMSL10/ItalicAngle -9.46/StemH 31/StemV 79/Type/FontDescriptor/XHeight 428>>endobj109 0 obj<</BaseFont/NPEIBC+CMTT10/Encoding 102 0 R/FirstChar 40/FontDescriptor 103 0 R/LastChar 144/Subtype/Type1/ToUnicode 105 0 R/Type/Font/Widths[525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525 525]>>endobj110 0 obj<</Differences[46/period 49/one/two/three/four/five/six/seven/eight 65/A/B/C/D/E/F/G/H/I 76/L/M/N/O/P 82/R/S/T/U 88/X/Y]/Type/Encoding>>endobj111 0 obj<</Differences[65/A]/Type/Encoding>>endobj112 0 obj<</Length 319/Subtype/Type1C>>stream
  NPEIBD+CMR7 '����������[ LNV������ 7;Computer Modern fonts were designed by Donald E. KnuthCMR7    "  A� {| ���S��B��U�F����xx��{��{��r�z_1g������Շ��_o�������������&�����oU�pg��ˍ��ֈ���wH�������������w��?�����n��a��Cy �	�
���ڛ8J�_�
%�o
endstreamendobj113 0 obj<</Ascent 689/CapHeight 671/CharSet(/space/A)/Descent -203/Flags 6/FontBBox[-286 -250 1122 967]/FontFile3 112 0 R/FontName/NPEIBD+CMR7/ItalicAngle 0/StemH 36/StemV 79/Type/FontDescriptor/XHeight 428>>endobj114 0 obj<</Filter/FlateDecode/Length 213>>stream
h�TP=��0��+<��"���z屇�-��������b�-�}����Wd�;]c�ƒa���Ꭽ%��`�s7e�)2���U�8(
!�<��]eK�g6ȖZX\��[���'vH2(K0��;*R�L�v=�z��Y���ȊZ�b����d�g��{���k3�bqwF+��6���m�w���-��%����B�	0 �h`
endstreamendobj115 0 obj<</BaseFont/NPEIBD+CMR7/Encoding 111 0 R/FirstChar 65/FontDescriptor 113 0 R/LastChar 65/Subtype/Type1/ToUnicode 114 0 R/Type/Font/Widths[843]>>endobj116 0 obj<</Length 283/Subtype/Type1C>>stream
  NPEIBE+CMMI10 -���J��k������ LNQ�-�2�6��� DJtrianglerightComputer Modern fonts were designed by Donald E. KnuthCMMI10   �  .� HI ���g����\�y������������u����w�w}�L}�w�������������[�"u��?�������a��Cy �	�
���3:33O��
endstreamendobj117 0 obj<</BaseFont/NPEIBE+CMMI10/Encoding 118 0 R/FirstChar 2/FontDescriptor 119 0 R/LastChar 2/Subtype/Type1/Type/Font/Widths[500]>>endobj118 0 obj<</Differences[2/triangleright]/Type/Encoding>>endobj119 0 obj<</Ascent 689/CapHeight 672/CharSet(/space/triangleright)/Descent -203/Flags 68/FontBBox[-32 -250 1048 750]/FontFile3 116 0 R/FontName/NPEIBE+CMMI10/ItalicAngle -14.04/StemH 31/StemV 72/Type/FontDescriptor/XHeight 439>>endobj120 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 152/Width 76>>stream
8;Z]_9+&HW$q4%f^VRCM[O2!H'1Ga74[ij.+U3c+!!%OO;Z=D/<0f(1VgA0)6\!fq
p*oZHj]ZMj:,V*1QS;7NV"VP#cedAb/o+ElG$(m4kF"hT4C[ME`&N3`R1jdEj2TJm
\f(PK!!&+P!<B+$0YI~>
endstreamendobj121 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 173/Width 76>>stream
8;Z]]>7,m7$q*te^VST`TWJDIl,-s[h",%F,@ls0!!(s/[Ofu'%pNr3I7G5MPd4P8
Bhg:_O=dfF4Ej$]NlqQSa'$f8=f)"qT%<?F\%#>;dD7k!IMstRZZSu8?Z,-mrZd6B
=VsK61nTe7DSEoFPo&"*=@`Dl!!!#7@35Jq`pWO~>
endstreamendobj122 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 243/Width 76>>stream
8;Z]!5n\\j$j6D,&,&(k"GK\kZr7+6#>^bLQ3+'!63na1NHeVu16f8^;W<duO98@)
[".,!_lg*O*oqCFRW5m\:kgfke1Y$^H^SG?>b`[W?`"D-65B_]T*.=0g,5qHZD#eB
f=`jlPksOb]sfn-id1_igX/63GOORU>_Q6`(J7h?;`?/95[3Jt(QR6?;qL!9r(Nn)
pWnP6a?DpOd*$&K^'XY_/oe<3h+B5!%O"&F)ZWD4?/Y~>
endstreamendobj123 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 106/Width 76>>stream
8;Z][_$L)b&-G2pDpWZT*;U71'bl@ipTc'$&\(s\\bU'fnq)tTpmko2g%)<Co^qiW
Y"!s;Nh*eYUc56lC0#Vh,6.]D!!$Z.!<=pZB`\~>
endstreamendobj124 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 264/Width 76>>stream
8;Z]!>7Q?0$q.`-%<af[gjjW$0A@1>kBn\,36o<1?j6T2!'h>E9"/igC&MF;CU&R:
n-I/mDpShP>=!P9G6kjT"eNeNI9d9hV46u)%TEsAgYns;p-S;b%9j#SGLMMXe`gYq
477@1fI5@SO<[7bC3OqnjCkjP's1>M4gNVPjINK#T%q$uBm*"Zs+[[`s#0+@?[BL2
B><3n0TD8_/fmXa(!XD)OhasFF?6%S5NgV`DCk"1BA-QaDbVDe,6\&hitqc=#7(>~>
endstreamendobj125 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 111/Width 76>>stream
8;Z]]9+&HW$jFH%s+7dV.J+hg95l&?c7D@O6[Y=/X=S\`LMIO9Sc9=L@SJV'%b[YC
oCI:@eXPfO'jI`i\dp@`2/C$hgi9P,z!$AZZ!0d`Mmf~>
endstreamendobj126 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 220/Width 76>>stream
8;Z]">n2B-%"id($Z[6sLb]$baMJ/RHgh9(;@3nV%KHJ/!)89jTAIJQ<o@#Q;)&AE
-R"ilENqKkoVW?p#WK"_aDC'.:^iR2:17JDCIMU@2>.$kip-`qmA@<GGKN[$?rfY#
l:[;B([6SAi(R`odF0P_rA"7*g2/0=KuaTjhPfLNB"*[rs(KtYYNi0mkdn-iMTM>K
&JII2E9*Ym=Ij4-]H/tE~>
endstreamendobj127 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 311/Width 76>>stream
8;Z]"!=j`9#Xs+?$hcV;`]74OeH$DZ]OZqC6N79IY1=ji,#dFAQ0#LG4^,UaGnc8s
I`S?rD<?.%FL!O-&=3f1Xf9&QQF]>3Yk$6TMh)q;9iT]u&QbIul#O=XYh:&QO#hD,
7MCSkkGrJBR8Q5G&PVYs!4"kqpuO7L$:DaHi(O9X#BmSX]sk[E`(65k7k_>W#KOCi
krjC'MZBY5AJT\N:oWL-j"k:iX@j6u(F7Vi.0m8;=g.lWGF[XrFWPt3m+SCZa`j*;
,4QiUVn@jumK8aRr2Jd&g/?NgBR#TI$;-I,!IP(OY[C92~>
endstreamendobj128 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 131/Width 76>>stream
8;Z]_>7/.g$q/Lrrs#g#g3sa3=JllUi&unq8=0rC!l0tp[*n)Am@@feb/M'9S]nF]
$b+c2[.MltQO5U""=rQ&g>`AO$N&EmW'7pUPH71JoYUO8\j2;Vz!!#d7!;ZP6[f~>
endstreamendobj129 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 41/Width 76>>stream
8;Z]LJH,ZM!5fq/_Wb=a!*'%"z!:ZEW!rsh\Y[m~>
endstreamendobj130 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 274/Width 76>>stream
8;Z]!;%eL($q0jh(X.$U6&K&ILnT?@hhUDTAi@7u&Kuf=6HCGQgp3K?CMg`-=_gT*
7;?eE6[X\3iuh25nVoji(StL;'d.k9Ias?U5Op<7/fAH$r_1fg)$CZ'eEW`Q(#ZfQ
CVPbi_k`p.0"O%:=U)iE,X5H6^El-keC<cY^P:Kqq&n,`i_>rJQ2lZ1i3YmQ*@H@+
FIZ*A*,h[dgg@p@3L"Q>cf[&EFd"EEq%>H:o51>"G"[(`>H'LF$K=P@l,DO:9#sW;
"p!?+UL4~>
endstreamendobj131 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 265/Width 76>>stream
8;Z]"@1If1#_ftl"hd[33Q4D*76nleUU<#FnGl'63f[^<E*X+P##Ln*9APUmp^0+)
&u1b2S"KDW2I%%S6=+2!49!heC$*j?/VXqaI*1O4q;'=NX8-;0pU-TXE,K(9;57r0
s'4);'k^]NKHVKcP#O/:Pcj9)_Phl).9]7#[fo_R1IRSn=!\/b(T"_07Yfn6ht"L\
"*([4:UYH#K>q)2ZWTDt:EVBDY,\d&!q)GA*,95D=-iO:E*_&u8R[Y>3.PK=`6-0o~>
endstreamendobj132 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 237/Width 76>>stream
8;Z]"%17V.%#&p*$gp'D'SU?LU>&W@&K+RCp=%KXK-5\t-lr,Ja#*"@g8VrC"M?Vb
Kbu[*.JFb3\h*$NLY+8lhe;+iBaKT-g(ZT7Qb.:NB&7n2f:!S'o6eI48kM:Ik%Yq&
>M#BlPO^&X0!lTBE32L<e%E)Iiia`lD\$jf#8*-W#V;EtVbaO;?)^JB%A4P<P,`#n
`?MT"]!dLD6F[E5IoGS0AN=B7"@49Q!2(@L4T~>
endstreamendobj133 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 276/Width 76>>stream
8;Z]!=Uos)$jF?E"hBX_>Y9^%MLCb35)]jmdfh!"a)NXd:$X*Gg^qbTBN5]sq&oJO
P=uc,>,%U?3*=':"^<it*C?lgUn1W"a>[+qc>7Kn2)7^<3Hi:0[WO2+CPS,OLQX]-
QVbkSK<V]CndXrpCacaN;pWbiL*@@KTR)b1gWOK\ALIGNc2atbpN0d/ncsRdee$A8
?.f<GHX*P:YJ#sR?E,W-J=fn5bYc\m>mHq<EuQ!-o>q4.,0&Vi+aI+EXR:3U1rjU+
c^>"<"qHG]~>
endstreamendobj134 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 283/Width 76>>stream
8;Z\u$OVP@$q,1:$g'L^kb:`HYq[=ja?ua<)^j[11)E@[$.8M$f`onaM]bR]HX`p-
@BnHuWZj^[T1=P)X;AK$+rK9ogn,LsHDTHjhEc6lC.g+b`%)JL0:_][<fRiCX_X+I
2-IT$r3/`sYX/ACT4,7t[[eOOU6;L,jREM.@L$0Gmk$g2$(gJ(O?Bs[E/\OlY#H,c
AIs=\EU-5#'%X2s@m27X^%HJMc#9&_=iVWI*h@%knsHhgA0n(uR->KliPKc&d+MN&
lGngW.!0h@!5HG)MZ~>
endstreamendobj135 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 224/Width 76>>stream
8;Z]"9+HIp$q.Z+"a_7!,[-lV;/mL1DkY)QR84u'KFecaPk(TR/JS(@[+#YgGt8kf
>*YCaQ&$)XNKWN"W3@,!Al@`BQD<XQQ)YP"9Up64f86u/0Gqm/R%;(:#T-MfC6)\6
\Q2Q2&+;A]qIL(Z[jAV2TtMP*9=X<DQK<aapD2$$H/PkHX;JFel,4+4l]).iC#uMq
5+m`L'Hr,@63na1cS>J@?=rC~>
endstreamendobj136 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 225/Width 76>>stream
8;Z]a5n8Pj$jG#7s+7qE#nTH97&?Rlr`b=;MeTo%TZdB9!-!jP_WC\4Wtd>uf4'_)
)bK?4^Q6qQe^@'QiZ_bnIuA@<f>Y@*ckB1'V;%80P]`3W1%3*BZa`cV<KB!n7[]->
`2KUV<'IX;,DNupN)8niK2o)JL3g;;>rH"(\UA@2c<'c?7NHI%HFNHh-FBjeO(X5W
.92B!`A#[bCYffAlAblFC/2iL~>
endstreamendobj137 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 160/Width 76>>stream
8;Z]^>7,mG#XD8]^Ho>-"@F9+@M()3cVMo@q&365PDKE4QhBu8r/\H-di'FEm49&H
UC]"?#\k><NsX+:),9GBl<DAq:[`^#c@&`'](=R"=5%uLS6+h@$cpu6O)OWrR@%2Y
:d^fLF%gfjX.TFmzRN2J1XsI&C~>
endstreamendobj138 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 215/Width 76>>stream
8;Z]!;%A4$&4G^\(Z[V7kUOOUYYZbEh?P6fa^fQ0SHAilJ0^<UAKU3E<Yd"3MR>/1
+FN$]'p%Qqd9#CdelKe#(>h&-rhI/0.;O4B7J<!I&<j0?8#HZPTr%h4@sa[?SJc;5
iP24O.q#l\]<?2'-`^fcUL\Sgb8j/c*uccZ)HEjFOo75o33A?m[qEU+SLsQB!WW5I
*mQNk)0lYk3/LoJ~>
endstreamendobj139 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 41/Width 76>>stream
8;Z]LJH,ZM!5fq/_Wb=a!*'%"z!:ZEW!rsh\Y[m~>
endstreamendobj140 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 255/Width 76>>stream
8;Z]"CCZ"?%#!gC([Q:q+MlU6%;6Qhm!OXer%Uke<YZd2&420ZJ99MK+)C7[E_j*=
\0<N#Hlo2L<.\)*r\Pgc;Xsj!3S^[99.uC+i.C/]\^dfN9Mh>%F`R`:($d`5B0aba
$(5DO&<5ps/04Gpkt=-^?gW-<@8)OGWF=)BlRQiqeL(7Q[8EtjBhF?E2]d_!ZNU*!
d1()53B;G(e/I<DpNcY%Rr.8&`0`4n?SQ@FU?#rHIhsd^9CNi"r.=2"~>
endstreamendobj141 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 127/Width 76>>stream
8;Z]]9+$2'$jFH's+7eATHZuQpc",!<SKreI;K*`=_l^DA6I,\+uXl)YG>^@:M))$
]hutllJ#[6ojZqP8qr5+qs5n`ahAjpO*YWnJO(AB:D*g2!!!!IC)?pVrRT?~>
endstreamendobj142 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 291/Width 76>>stream
8;Z]"CC\9*#_]bg"hDojCMU.X6tKr;Hp@I$-8S3C,!(bE30(lX!66N0`j>dQ_cj!I
kXBlS0iU:MD5;98%-2]1@?C8*ecI8a?$qCiWtm9<$.9<RaE,AV?^"NE);1I,orDMA
Kn#Uii6,Sg<p!M:+AN0K*NZBBn^QtirH1]3qVj%4^IN95%HBokL=r=G4D^c3jt:Kp
H=rd@`gcm%i]R+$Cp"PN>4.,gUT711@[n)\pVgdWm6LnZr(J^AI4H#QY"6)VqEa9j
Zl[^_G0&%LX26pC2LG]/U79g#~>
endstreamendobj143 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 290/Width 76>>stream
8;Z]">7Q?0$q/eK#.pFA1`EFedUJVRm*Q=h>%\'/6'NY:b`j=ED]B5N`/dd@n=pr"
>!e(%n7sKnX**UeM-a1d<g8AMaaVoX&$+of:f"5qc[2sf%`Hhk@pk<f&dE[c34%r!
]Uki[[.+NR?B:\f`<B+=GrKlq!Z1nN4^JC@[Q'VB+WN"mp=4BTkZa+Tn'\rm^fO63
OVK=!rO^YG;(9Zcl2*<n4uWal/:p]ZEC?0c<h:Vp(e7fpb8fND_B*Xs]!g2O#/klC
gt]+t?5Mi\8WmTq^B">AU3B-~>
endstreamendobj144 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 97/Width 76>>stream
8;Z]\YmnJk$j8ih?guYR9nX]Y.KhBlA@ZaN[M,=0ep;bH-V6O(`l7NBgTdV'rjeJ5
Q1SUcc$!u'iF`,j!!!"`nI,DVCaIL~>
endstreamendobj145 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 41/Width 76>>stream
8;Z]LJH,ZM!5fq/_Wb=a!*'%"z!:ZEW!rsh\Y[m~>
endstreamendobj146 0 obj<</BitsPerComponent 8/ColorSpace 79 0 R/Filter[/ASCII85Decode/FlateDecode]/Height 99/Length 212/Width 76>>stream
8;Z]!$OVD,&4H9l(\BaW)4L?R<&TI)La]93Ai;4'!!!"lNk>jtI%s##cHUF$%6>FC
fTi&`GNWCjnjpI4PfhSt_`Zt-7#+D_OZ1pKFSI`)[5(f7ZK6Hk!e8X5Ai7XfT2nD?
=-4Spkuko*qW94jI>[<^Fk`:/h%qU<W&(&>fb6,!,RncCS9=`8U^pY=#64aS7H%;J
?q64X!6uafmf~>
endstreamendobj147 0 obj<</Nums[0 148 0 R 4 149 0 R]>>endobj148 0 obj<</S/D>>endobj149 0 obj<</S/D>>endobj150 0 obj<</Count 27/Kids[151 0 R 152 0 R 153 0 R]/Type/Pages>>endobj151 0 obj<</Count 10/Kids[158 0 R 1 0 R 4 0 R 7 0 R 10 0 R 13 0 R 16 0 R 19 0 R 22 0 R 25 0 R]/Parent 150 0 R/Type/Pages>>endobj152 0 obj<</Count 10/Kids[28 0 R 31 0 R 34 0 R 37 0 R 40 0 R 43 0 R 46 0 R 49 0 R 52 0 R 55 0 R]/Parent 150 0 R/Type/Pages>>endobj153 0 obj<</Count 7/Kids[58 0 R 61 0 R 64 0 R 67 0 R 70 0 R 73 0 R 76 0 R]/Parent 150 0 R/Type/Pages>>endobj154 0 obj<</Length 3801/Subtype/XML/Type/Metadata>>stream
<?xpacket begin="﻿" id="W5M0MpCehiHzreSzNTczkc9d"?>
<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="Adobe XMP Core 4.2.1-c041 52.342996, 2008/05/07-20:48:00        ">
   <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
      <rdf:Description rdf:about=""
            xmlns:dc="http://purl.org/dc/elements/1.1/">
         <dc:format>application/pdf</dc:format>
         <dc:title>
            <rdf:Alt>
               <rdf:li xml:lang="x-default">guide</rdf:li>
            </rdf:Alt>
         </dc:title>
         <dc:creator>
            <rdf:Seq>
               <rdf:li>Paul C. Anagnostopoulos 101 2008-04-12 11:19:18</rdf:li>
            </rdf:Seq>
         </dc:creator>
         <dc:description>
            <rdf:Alt>
               <rdf:li xml:lang="x-default">TeX output 2011.11.08:1249</rdf:li>
            </rdf:Alt>
         </dc:description>
      </rdf:Description>
      <rdf:Description rdf:about=""
            xmlns:xmp="http://ns.adobe.com/xap/1.0/">
         <xmp:CreateDate>2011-11-08T12:50:14Z</xmp:CreateDate>
         <xmp:CreatorTool>DVIPSONE 2.3.2.124 http://www.YandY.com</xmp:CreatorTool>
         <xmp:ModifyDate>2011-11-08T12:50:18-05:00</xmp:ModifyDate>
      </rdf:Description>
      <rdf:Description rdf:about=""
            xmlns:pdf="http://ns.adobe.com/pdf/1.3/">
         <pdf:Producer>Acrobat Distiller 9.0.0 (Windows)</pdf:Producer>
      </rdf:Description>
      <rdf:Description rdf:about=""
            xmlns:xmpMM="http://ns.adobe.com/xap/1.0/mm/">
         <xmpMM:DocumentID>uuid:91fb9e0e-5b26-4004-b806-72799be99916</xmpMM:DocumentID>
         <xmpMM:InstanceID>uuid:7c36d48f-f282-4370-9c46-7e104e487fb8</xmpMM:InstanceID>
      </rdf:Description>
   </rdf:RDF>
</x:xmpmeta>
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
                           
<?xpacket end="w"?>
endstreamendobj155 0 obj<</Author(Paul C. Anagnostopoulos 101 2008-04-12 11:19:18)/CreationDate(D:20111108125014Z)/Creator(DVIPSONE 2.3.2.124 http://www.YandY.com)/ModDate(D:20111108125018-05'00')/Producer(Acrobat Distiller 9.0.0 \(Windows\))/Subject(TeX output 2011.11.08:1249)/Title(guide)>>endobjxref
0 156
0000000000 65535 f
0000012731 00000 n
0000012873 00000 n
0000012977 00000 n
0000014141 00000 n
0000014283 00000 n
0000014388 00000 n
0000015458 00000 n
0000015600 00000 n
0000015716 00000 n
0000016174 00000 n
0000016319 00000 n
0000016447 00000 n
0000018310 00000 n
0000018455 00000 n
0000018562 00000 n
0000019183 00000 n
0000019328 00000 n
0000019468 00000 n
0000020506 00000 n
0000020651 00000 n
0000020780 00000 n
0000022695 00000 n
0000022840 00000 n
0000022958 00000 n
0000023784 00000 n
0000023929 00000 n
0000023964 00000 n
0000024040 00000 n
0000024185 00000 n
0000024292 00000 n
0000025334 00000 n
0000025479 00000 n
0000025619 00000 n
0000027278 00000 n
0000027423 00000 n
0000027563 00000 n
0000029207 00000 n
0000029352 00000 n
0000029492 00000 n
0000031219 00000 n
0000031364 00000 n
0000031493 00000 n
0000033472 00000 n
0000033617 00000 n
0000033767 00000 n
0000035144 00000 n
0000035289 00000 n
0000035418 00000 n
0000036508 00000 n
0000036653 00000 n
0000036793 00000 n
0000037634 00000 n
0000037779 00000 n
0000037897 00000 n
0000038806 00000 n
0000038951 00000 n
0000038986 00000 n
0000039062 00000 n
0000039207 00000 n
0000039326 00000 n
0000040426 00000 n
0000040571 00000 n
0000040678 00000 n
0000041329 00000 n
0000041474 00000 n
0000041592 00000 n
0000043204 00000 n
0000043349 00000 n
0000043489 00000 n
0000045914 00000 n
0000046059 00000 n
0000046177 00000 n
0000046667 00000 n
0000046812 00000 n
0000046847 00000 n
0000046923 00000 n
0000047068 00000 n
0000047208 00000 n
0000048439 00000 n
0000048487 00000 n
0000049001 00000 n
0000053652 00000 n
0000054396 00000 n
0000055136 00000 n
0000055525 00000 n
0000055930 00000 n
0000056215 00000 n
0000056624 00000 n
0000057164 00000 n
0000057495 00000 n
0000057720 00000 n
0000065697 00000 n
0000066107 00000 n
0000066772 00000 n
0000066988 00000 n
0000067062 00000 n
0000067272 00000 n
0000067741 00000 n
0000068120 00000 n
0000068462 00000 n
0000072431 00000 n
0000078484 00000 n
0000078850 00000 n
0000079352 00000 n
0000079689 00000 n
0000080055 00000 n
0000080391 00000 n
0000084043 00000 n
0000084353 00000 n
0000084934 00000 n
0000085088 00000 n
0000085142 00000 n
0000085528 00000 n
0000085746 00000 n
0000086030 00000 n
0000086192 00000 n
0000086542 00000 n
0000086686 00000 n
0000086751 00000 n
0000086988 00000 n
0000087283 00000 n
0000087599 00000 n
0000087985 00000 n
0000088234 00000 n
0000088641 00000 n
0000088895 00000 n
0000089258 00000 n
0000089712 00000 n
0000089986 00000 n
0000090169 00000 n
0000090586 00000 n
0000090994 00000 n
0000091374 00000 n
0000091793 00000 n
0000092219 00000 n
0000092586 00000 n
0000092954 00000 n
0000093257 00000 n
0000093615 00000 n
0000093798 00000 n
0000094196 00000 n
0000094466 00000 n
0000094900 00000 n
0000095333 00000 n
0000095572 00000 n
0000095755 00000 n
0000096110 00000 n
0000096158 00000 n
0000096184 00000 n
0000096210 00000 n
0000096282 00000 n
0000096413 00000 n
0000096546 00000 n
0000096657 00000 n
0000100537 00000 n
trailer
<</Size 156/ID[<3E6AA0EC231AA6DFAB1A9C9D6D9289AC><866409EB9AB4EF49A0013B161CB331DD>]>>
startxref
116
%%EOF
                                                                                                                                                                                                                                                                                                                                                                                   doc/ICFP2016/fig-typing.tex                                                                         0000664 0001750 0001750 00000013637 13216017506 015547  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[t]
  Typing for expressions, $\Delta;\Gamma \vdash e: T$
  %
  \begin{gather*}
    % 1 _ LAMBDA
    \frac{\Delta \vdash \Gamma \quad \un_\Delta(\Gamma)}{\Delta;\Gamma \vdash () \colon \unitk}
    \quad
    % VAR
    \frac{
      \Delta \vdash \Gamma \quad
      \un_\Delta(\Gamma) \quad
      \Delta \vdash \vec T :: \vec \kind
    }{
      \Delta;\Gamma,a\colon \forall \vec\alpha::\vec\kind. T
      \vdash a : T\subs{\vec T}{\vec \alpha}}
    \\
    % LET-poly
    \frac{
      %\begin{array}{c}
      \Delta, \vec\alpha :: \vec\kind; \Gamma_1 \vdash e_1 : T_1 \quad
      \Delta; \Gamma_2, a : \forall \vec\alpha::\vec\kind. T_1 \vdash
      e_2 : T \quad
      \vec\alpha \notin \Delta
      %\end{array}
    }{
      \Delta;\Gamma_1\circ \Gamma_2 \vdash \letin{a}{e_1}{e_2} : T
    }
    \\
    % FIX
    \frac{
      %\begin{array}{c}
        \Delta, \vec\alpha :: \vec\kind;\Gamma, a\colon  \forall
        \vec\alpha::\vec\kind. T \vdash e : T  \quad
        \un_\Delta(\Gamma) \quad
        \vec\alpha \notin \Delta
      %\end{array}
    }{
      \Delta;\Gamma \vdash \fix ae :  \forall \vec\alpha::\vec\kind .T
    }
    \\
    % Arrow Intro / Elim
    \frac{
      \Delta;\Gamma,a\colon T_1 \vdash e : T_2
      \quad
      \un_\Delta(\Gamma)
      \quad
      \Delta \vdash T_1,T_2 ::\kindt^\Linear
    }{
      \Delta;\Gamma \vdash \lambda a.e : T_1 \rightarrow T_2
    }
    \\
    \frac{
      \Delta;\Gamma,a\colon T_1 \vdash e : T_2::\kindt^\Linear
      \quad
      \Delta \vdash T_1,T_2 ::\kindt^\Linear
    }{
      \Delta;\Gamma \vdash \lambda a.e : T_1 \multimap T_2
    }
    \\
    \frac{
      \Delta;\Gamma_1 \vdash e_1 : T_1 \multimap T_2 \quad \Delta;\Gamma_2
      \vdash e_2 : T_1
    }{
      \Delta;\Gamma_1\circ  \Gamma_2 \vdash e_1 e_2 : T_2
    }
    % \;\;%\quad
    % \frac{
    %   \Delta;\Gamma \vdash e : T_1 \rightarrow T_2
    % }{
    %   \Delta;\Gamma \vdash e : T_1 \multimap T_2
    % }
     \\
    \frac{
      \Delta;\Gamma_1 \vdash e_1 : T_1 \rightarrow T_2 \quad \Delta;\Gamma_2
      \vdash e_2 : T_1
    }{
      \Delta;\Gamma_1\circ  \Gamma_2 \vdash e_1 e_2 : T_2
    }
   \\
    % Tensor Intro / Elim
    \frac{
      \Delta;\Gamma_1 \vdash e_1 : T_1
      \quad
      \Delta;\Gamma_2 \vdash e_2 : T_2
      \quad
      \Delta \vdash T_1,T_2 ::\kindt^\Linear
    }{
      \Delta;\Gamma_1\circ \Gamma_2 \vdash (e_1,e_2) : T_1 \otimes T_2
    }
    \\
    \frac{
      \Delta;\Gamma_1 \vdash e_1 : T_1 \otimes T_2
      \quad
      \Delta;\Gamma_2, a\colon T_1, b\colon T_2 \vdash e_2: U
    }{
      \Delta;\Gamma_1 \circ \Gamma_2 \vdash \letin{a,b}{e_1}{e_2} : U
    }
    \\
    % Variant Intro / Elim
    \frac{
      \Delta;\Gamma \vdash e : T_j \quad j\in I \quad
      \Delta\vdash T_i::\kindt^\Linear
    }{
      \Delta;\Gamma \vdash \inject {l_j} e : [l_i\colon T_i]_{i\in I}
    }
    \\
    \frac{
      \Delta;\Gamma_1 \vdash e : [l_i\colon T_i]
      \quad
      \Delta;\Gamma_2 \vdash e_i : T_i \multimap T
    }{
      \Delta;\Gamma_1\circ \Gamma_2 \vdash \match e {l_i\colon e_i} : T
    }
    % \\
    % % Forall Intro / Elim
    % \frac{
    %   \Delta;\Gamma \vdash e : T
    %   \quad 
    %   \alpha \notin \Delta,\Gamma
    %   \quad 
    %   \Delta,\alpha::\kind \vdash T :: \kindsch^\Linear
    %   \quad
    %   \kind \le \kindt^\Linear
    % }{
    %   \Delta;\Gamma \vdash e : \forall \alpha::\kind.T
    % }
    % \\
    % \frac{
    %   \Delta;\Gamma \vdash e : \forall\alpha::\kind.T_1
    %   \quad
    %   \Delta \vdash T_2::\kind
    % }{
    %   \Delta;\Gamma \vdash e : T_1 \subs{T_2} \alpha
    % }
    \\
    % 2 _ SESSIONS
    % New
    \frac{
      \Delta \vdash \Gamma
      \quad
      \Delta \vdash T::\kinds^\Linear
    }{
      \Delta;\Gamma \vdash \newk : T\otimes \dual T
    }
    \quad
    % Send/Receive
    \frac{
      \Delta \vdash \Gamma
      \quad
      \Delta \vdash T::\kinds^\Linear
    }{
      \Delta;\Gamma\vdash \sendk : B \rightarrow \;!B;T \rightarrow T
    }
    \\
    \frac{
      \Delta \vdash \Gamma
      \quad
      \Delta \vdash T::\kinds^\Linear
    }{
      \Delta;\Gamma \vdash \recvk : \;?B;T \rightarrow (B \otimes T)
    }
    \\
    % Select Intro
    \frac{
      \Delta;\Gamma \vdash e : \oplus\{l_i\colon T_i\}_{i\in I}
      \quad
      j\in I
    }{
      \Delta;\Gamma \vdash \select {l_j} e \colon T_j
    }
    \\
    % Case Intro
    \frac{
      \Delta;\Gamma_1 \vdash e : \&\{l_i\colon T_i\} \quad \Delta;\Gamma_2 \vdash e_i
      : T_i \multimap T
    }{
      \Delta;\Gamma_1\circ  \Gamma_2 \vdash \case e {l_i\colon e_i} : T
    }
    \\
    % 3 _ CONCURRENCY
    % Fork
    \frac{
      \Delta;\Gamma \vdash e : T
      \quad
      \un_\Delta(T)
    }{
      \Delta;\Gamma \vdash \forkk\,e : \unitk
    }
    \quad
    % 4 _ STRUCTURAL (the forall rules are structural as well)
    % Weak, Copy, Equiv
    % \frac{
    %   \Delta;\Gamma \vdash e : T_1 \quad \un_\Delta(T_2) \quad a\notin\Gamma
    % }{
    %   \Delta;\Gamma, a\colon T_2 \vdash e : T_1
    % }
    % \\
    % \frac{
    %   \Delta;\Gamma,a\colon T_1, a\colon T_1 \vdash e : T_2 \quad \un_\Delta(T_1)
    % }{
    %   \Delta;\Gamma, a\colon T_1 \vdash e : T_2
    % }
    % \\
    \frac{
      \Delta;\Gamma \vdash e : T_1 \quad \Delta \vdash T_1 \TypeEquiv T_2
    }{
      \Delta;\Gamma \vdash e : T_2
    }
  \end{gather*}
  %
   Typing for processes, $\Delta;\Gamma \vdash p$
  \begin{gather*}
    \frac{
      \Delta;\Gamma \vdash e : T \quad \un_\Delta(T)
    }{
      \Delta;\Gamma \vdash e
    }
    \\
    \frac{
      \Delta;\Gamma_1 \vdash P_1 \quad \Delta;\Gamma_2 \vdash P_2
    }{
      \Delta;\Gamma_1\circ  \Gamma_2 \vdash p_1 \mid p_2
    }
    \quad
    \frac{
      \Delta;\Gamma, a\colon T, b \colon \dual T \vdash p
            \quad
      \Delta \vdash T::\kinds^\Linear
    }{
      \Delta;\Gamma \vdash (\new a,b)p
    }
  \end{gather*}
  %
  \caption{Typing for expressions and processes}
  \label{fig:typing}
\end{figure}
%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                 doc/ICFP2016/biblio.bib                                                                             0000664 0001750 0001750 00000055567 13216017506 014676  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               @InBook{ walker:substructural-type-systems,
  _ditor = {Benjamin C. Pierce},
  author = 	 {David Walker},
  title = 	 {Advanced Topics in Types and Programming Languages},
  chapter = 	 {Substructural Type Systems},
  publisher = 	 {MIT Press},
  year = 	 {2005}
}

@inproceedings{DBLP:conf/parle/Vasconcelos94,
  author    = {Vasco Thudichum Vasconcelos},
  title     = {Predicative Polymorphism in Pi-Calculus},
  booktitle = {PARLE},
  _booktitle = {{PARLE} '94: Parallel Architectures and Languages Europe, 6th International
               {PARLE} Conference, Athens, Greece, July 4-8, 1994, Proceedings},
  pages     = {425--437},
  year      = {1994},
  crossref  = {DBLP:conf/parle/1994},
  _url       = {http://dx.doi.org/10.1007/3-540-58184-7_120},
  _doi       = {10.1007/3-540-58184-7_120},
  timestamp = {Tue, 14 Jun 2011 20:35:25 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/parle/Vasconcelos94},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@proceedings{DBLP:conf/parle/1994,
  _editor    = {Constantine Halatsis and
               Dimitris G. Maritsas and
               George Philokyprou and
               Sergios Theodoridis},
  title     = {{PARLE} '94: Parallel Architectures and Languages Europe, 6th International
               {PARLE} Conference, Athens, Greece, July 4-8, 1994, Proceedings},
  series    = {LNCS},
  volume    = {817},
  publisher = {Springer},
  year      = {1994},
  _isbn      = {3-540-58184-7},
  timestamp = {Fri, 01 Mar 2002 07:44:57 +0100},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/parle/1994},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@article{DBLP:journals/toplas/CarboneHY12,
  author    = {Marco Carbone and
               Kohei Honda and
               Nobuko Yoshida},
  title     = {Structured Communication-Centered Programming for Web Services},
  journal   = {{ACM} Trans. Program. Lang. Syst.},
  volume    = 34,
  number    = 2,
  pages     = 8,
  year      = 2012,
  _url       = {http://doi.acm.org/10.1145/2220365.2220367},
  _doi       = {10.1145/2220365.2220367},
  timestamp = {Tue, 21 Aug 2012 18:24:25 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/toplas/CarboneHY12},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/popl/GayVRGC10,
  author    = {Simon J. Gay and
               Vasco Thudichum Vasconcelos and
               Ant{\'{o}}nio Ravara and
               Nils Gesbert and
               Alexandre Z. Caldeira},
  title     = {Modular session types for distributed object-oriented programming},
  booktitle = {POPL},
  _booktitle = {Proceedings of the 37th {ACM} {SIGPLAN-SIGACT} Symposium on Principles
               of Programming Languages, {POPL} 2010, Madrid, Spain, January 17-23,
               2010},
  pages     = {299--312},
  year      = {2010},
  crossref  = {DBLP:conf/popl/2010},
  _url       = {http://doi.acm.org/10.1145/1706299.1706335},
  _doi       = {10.1145/1706299.1706335},
  timestamp = {Tue, 22 May 2012 15:24:56 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/popl/GayVRGC10},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@proceedings{DBLP:conf/popl/2010,
  _editor    = {Manuel V. Hermenegildo and
               Jens Palsberg},
  title     = {Proceedings of the 37th {ACM} {SIGPLAN-SIGACT} Symposium on Principles
               of Programming Languages, {POPL} 2010, Madrid, Spain, January 17-23,
               2010},
  publisher = {{ACM}},
  year      = {2010},
  _url       = {http://dl.acm.org/citation.cfm?id=1706299},
  _isbn      = {978-1-60558-479-9},
  timestamp = {Tue, 22 May 2012 15:24:56 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/popl/2010},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/concur/CairesP10,
  author    = {Lu{\'{\i}}s Caires and
               Frank Pfenning},
  title     = {Session Types as Intuitionistic Linear Propositions},
  booktitle = {CONCUR},
  _booktitle = {{CONCUR} 2010 - Concurrency Theory, 21th International Conference,
               {CONCUR} 2010, Paris, France, August 31-September 3, 2010. Proceedings},
  pages     = {222--236},
  year      = {2010},
  crossref  = {DBLP:conf/concur/2010},
  _url       = {http://dx.doi.org/10.1007/978-3-642-15375-4_16},
  _doi       = {10.1007/978-3-642-15375-4_16},
  timestamp = {Thu, 26 Aug 2010 16:46:17 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/concur/CairesP10},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@proceedings{DBLP:conf/concur/2010,
  _editor    = {Paul Gastin and
               Fran{\c{c}}ois Laroussinie},
  title     = {{CONCUR} 2010 - Concurrency Theory, 21th International Conference,
               {CONCUR} 2010, Paris, France, August 31-September 3, 2010. Proceedings},
  series    = {LNCS},
  volume    = {6269},
  publisher = {Springer},
  year      = {2010},
  _url       = {http://dx.doi.org/10.1007/978-3-642-15375-4},
  _doi       = {10.1007/978-3-642-15375-4},
  _isbn      = {978-3-642-15374-7},
  timestamp = {Thu, 26 Aug 2010 16:45:23 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/concur/2010},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@article{DBLP:journals/jfp/Wadler14,
  author    = {Philip Wadler},
  title     = {Propositions as sessions},
  journal   = {J. Funct. Program.},
  volume    = 24,
  number    = {2-3},
  pages     = {384--418},
  year      = 2014,
  _url       = {http://dx.doi.org/10.1017/S095679681400001X},
  _doi       = {10.1017/S095679681400001X},
  timestamp = {Mon, 26 May 2014 10:31:52 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/jfp/Wadler14},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/esop/HondaVK98,
  author    = {Kohei Honda and
               Vasco Thudichum Vasconcelos and
               Makoto Kubo},
  title     = {Language Primitives and Type Discipline for Structured Communication-Based
               Programming},
  booktitle = {ESOP},
  _booktitle = {Programming Languages and Systems - ESOP'98, 7th European Symposium
               on Programming, Held as Part of the European Joint Conferences on
               the Theory and Practice of Software, ETAPS'98, Lisbon, Portugal, March
               28 - April 4, 1998, Proceedings},
  pages     = {122--138},
  year      = {1998},
  crossref  = {DBLP:conf/esop/1998},
  _url       = {http://dx.doi.org/10.1007/BFb0053567},
  _doi       = {10.1007/BFb0053567},
  timestamp = {Sun, 20 Sep 2009 17:24:17 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/esop/HondaVK98},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@proceedings{DBLP:conf/esop/1998,
  _editor    = {Chris Hankin},
  title     = {Programming Languages and Systems - ESOP'98, 7th European Symposium
               on Programming, Held as Part of the European Joint Conferences on
               the Theory and Practice of Software, ETAPS'98, Lisbon, Portugal, March
               28 - April 4, 1998, Proceedings},
  series    = {LNCS},
  volume    = {1381},
  publisher = {Springer},
  year      = {1998},
  _isbn      = {3-540-64302-8},
    timestamp = {Wed, 17 Apr 2002 09:38:10 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/esop/1998},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@article{DBLP:journals/mscs/Gay08,
  author    = {Simon J. Gay},
  title     = {Bounded polymorphism in session types},
  journal   = {Mathematical Structures in Computer Science},
  volume    = {18},
  number    = {5},
  pages     = {895--930},
  year      = {2008},
  _url       = {http://dx.doi.org/10.1017/S0960129508006944},
  _doi       = {10.1017/S0960129508006944},
  timestamp = {Mon, 04 May 2009 22:12:24 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/mscs/Gay08},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{BonoPadovaniTosatto13,
  volume = 7892,
  author = {Viviana Bono and Luca Padovani and Andrea Tosatto},
  series = {LNCS},
  booktitle = {FORTE},
  _booktitle = {Proceedings of the 2013 IFIP Joint International Conference on
               Formal Techniques for Distributed Systems (FORTE'13)},
  _url = {http://www.di.unito.it/~padovani/Papers/BonoPadovaniTosatto13.pdf},
  abstract = { Copyless message passing is a communication paradigm in which
              only pointers are exchanged between sender and receiver processes.
              Because of its nature, this paradigm requires that messages are
              treated as linear resources. Yet, even linear type systems leave
              room for scenarios where apparently well-typed programs may leak
              memory. In this work we develop a polymorphic type system for
              leak-free copyless messaging in a functional setting, where
              first-class functions can be used as messages. },
  title = {Polymorphic Types for Leak Detection in a Session-Oriented
           Functional Language},
  publisher = {Springer},
  year = 2013,
  pages = {83-98},
  _doi = {10.1007/978-3-642-38592-6\_7}
}

@article{MacQueen198695,
title = "An ideal model for recursive polymorphic types",
journal = "Information and Control",
volume = 71,
number = "1-2",
pages = "95--130",
year = 1986,
_issn = "0019-9958",
_doi = "http://dx.doi.org/10.1016/S0019-9958(86)80019-5",
_url = "http://www.sciencedirect.com/science/article/pii/S0019995886800195",
author = "David MacQueen and Gordon Plotkin and Ravi Sethi"
}

@Article{ bernardi.hennessy:using-contracts-model-session-types,
  author = 	 {Giovanni Bernardi and Matthew Hennessy},
  title = 	 {Using higher-order contracts to model session types},
  journal = 	 {Logical Methods in Computer Science},
  year = 	 {2016},
  volume = 	 {12},
  number = 	 {2:10},
  pages = 	 {1--43},
  OPTmonth = 	 {},
  OPTnote = 	 {},
  OPTannote = 	 {}
}

@InProceedings{ bernardi.hennessy:using-contracts-model-session-types-concur,
  author = 	 {Giovanni Bernardi and Matthew Hennessy},
  title = 	 {Using higher-order contracts to model session types},
  booktitle = {CONCUR},
  year = 	 {2014},
  OPTeditor = 	 {},
  volume = 	 {8704},
  OPTnumber = 	 {},
  series = 	 {LNCS},
  pages = 	 {387--401},
  OPTmonth = 	 {},
  OPTaddress = 	 {},
  OPTorganization = {},
  publisher = {Springer},
  OPTnote = 	 {},
  OPTannote = 	 {}
}

@article{DBLP:journals/acta/GayH05,
  author    = {Simon J. Gay and
               Malcolm Hole},
  title     = {Subtyping for session types in the pi calculus},
  journal   = {Acta Informatic\ae},
  volume    = {42},
  number    = {2-3},
  year      = {2005},
  pages     = {191-225},
  ee        = {http://dx.doi.org/10.1007/s00236-005-0177-z},
  bibsource = {DBLP, http://dblp.uni-trier.de}
}

@inproceedings{DBLP:conf/icalp/Senizergues97,
  author    = {G{\'{e}}raud S{\'{e}}nizergues},
  _editor    = {Pierpaolo Degano and
               Roberto Gorrieri and
               Alberto Marchetti{-}Spaccamela},
  title     = {The Equivalence Problem for Deterministic Pushdown Automata is Decidable},
  booktitle = {ICALP},
  _booktitle = {Automata, Languages and Programming, 24th International Colloquium,
               ICALP'97, Bologna, Italy, 7-11 July 1997, Proceedings},
  series    = {LNCS},
  volume    = {1256},
  pages     = {671--681},
  publisher = {Springer},
  year      = {1997},
  _url       = {http://dx.doi.org/10.1007/3-540-63165-8_221},
  _doi       = {10.1007/3-540-63165-8_221},
  timestamp = {Sun, 20 Sep 2009 10:49:56 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/icalp/Senizergues97},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:journals/corr/Loding14,
  author    = {Christof L{\"{o}}ding},
  _editor    = {Zolt{\'{a}}n {\'{E}}sik and
               Zolt{\'{a}}n F{\"{u}}l{\"{o}}p},
  title     = {Decision Problems for Deterministic Pushdown Automata on Infinite
               Words},
  booktitle = {Proceedings 14th International Conference on Automata and Formal Languages,
               {AFL} 2014, Szeged, Hungary, May 27-29, 2014.},
  series    = {{EPTCS}},
  volume    = {151},
  pages     = {55--73},
  year      = {2014},
  _url       = {http://dx.doi.org/10.4204/EPTCS.151.4},
  _doi       = {10.4204/EPTCS.151.4},
  timestamp = {Mon, 02 Jun 2014 18:06:07 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/corr/Loding14},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/mfcs/LodingR12,
  author    = {Christof L{\"{o}}ding and
               Stefan Repke},
  _editor    = {Branislav Rovan and
               Vladimiro Sassone and
               Peter Widmayer},
  title     = {Regularity Problems for Weak Pushdown {\(\omega\)}-Automata and Games},
  booktitle = {MFCS},
  _booktitle = {Mathematical Foundations of Computer Science 2012 - 37th International
               Symposium, {MFCS} 2012, Bratislava, Slovakia, August 27-31, 2012.
               Proceedings},
  series    = {LNCS},
  volume    = {7464},
  pages     = {764--776},
  publisher = {Springer},
  year      = {2012},
  _url       = {http://dx.doi.org/10.1007/978-3-642-32589-2_66},
  _doi       = {10.1007/978-3-642-32589-2_66},
  timestamp = {Sun, 12 Aug 2012 18:27:36 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/mfcs/LodingR12},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@article{DBLP:journals/jfp/GayV10,
  author    = {Simon J. Gay and
               Vasco Thudichum Vasconcelos},
  title     = {Linear type theory for asynchronous session types},
  journal   = {J. Funct. Program.},
  volume    = {20},
  number    = {1},
  pages     = {19--50},
  year      = {2010},
  _url       = {http://dx.doi.org/10.1017/S0956796809990268},
  _doi       = {10.1017/S0956796809990268},
  timestamp = {Mon, 22 Mar 2010 12:02:58 +0100},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/jfp/GayV10},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@article{DBLP:journals/tcs/Courcelle83,
  author    = {Bruno Courcelle},
  title     = {Fundamental Properties of Infinite Trees},
  journal   = tcs,
  volume    = 25,
  pages     = {95--169},
  year      = 1983,
  _url       = {http://dx.doi.org/10.1016/0304-3975(83)90059-2},
  _doi       = {10.1016/0304-3975(83)90059-2},
  timestamp = {Wed, 07 Sep 2011 12:13:22 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/tcs/Courcelle83},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/concur/Honda93,
  author    = {Kohei Honda},
  _editor    = {Eike Best},
  title     = {Types for Dyadic Interaction},
  booktitle = {CONCUR},
  _booktitle = {{CONCUR} '93, 4th International Conference on Concurrency Theory,
               Hildesheim, Germany, August 23-26, 1993, Proceedings},
  series    = {LNCS},
  volume    = {715},
  pages     = {509--523},
  publisher = {Springer},
  year      = {1993},
  _url       = {http://dx.doi.org/10.1007/3-540-57208-2_35},
  _doi       = {10.1007/3-540-57208-2_35},
  timestamp = {Sun, 27 May 2012 19:01:06 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/concur/Honda93},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/parle/TakeuchiHK94,
  author    = {Kaku Takeuchi and
               Kohei Honda and
               Makoto Kubo},
  _editor    = {Constantine Halatsis and
               Dimitris G. Maritsas and
               George Philokyprou and
               Sergios Theodoridis},
  title     = {An Interaction-based Language and its Typing System},
  booktitle = {PARLE},
  _booktitle = {{PARLE} '94: Parallel Architectures and Languages Europe, 6th International
               {PARLE} Conference, Athens, Greece, July 4-8, 1994, Proceedings},
  series    = {LNCS},
  volume    = {817},
  pages     = {398--413},
  publisher = {Springer},
  year      = {1994},
  _url       = {http://dx.doi.org/10.1007/3-540-58184-7_118},
  _doi       = {10.1007/3-540-58184-7_118},
  timestamp = {Tue, 14 Jun 2011 20:35:25 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/parle/TakeuchiHK94},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/pldi/Reppy91,
  author    = {John H. Reppy},
  _editor    = {David S. Wise},
  title     = {{CML:} {A} Higher-Order Concurrent Language},
  booktitle = {PLDI},
  _booktitle = {Proceedings of the {ACM} SIGPLAN'91 Conference on Programming Language
               Design and Implementation (PLDI), Toronto, Ontario, Canada, June 26-28,
               1991},
  pages     = {293--305},
  publisher = {{ACM}},
  year      = {1991},
  _url       = {http://doi.acm.org/10.1145/113445.113470},
  _doi       = {10.1145/113445.113470},
  timestamp = {Mon, 21 May 2012 16:19:53 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/pldi/Reppy91},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}
                  
@inproceedings{DBLP:conf/popl/HondaYC08,
  author    = {Kohei Honda and
               Nobuko Yoshida and
               Marco Carbone},
  _editor    = {George C. Necula and
               Philip Wadler},
  title     = {Multiparty asynchronous session types},
  booktitle = {POPL},
  _booktitle = {Proceedings of the 35th {ACM} {SIGPLAN-SIGACT} Symposium on Principles
               of Programming Languages, {POPL} 2008, San Francisco, California,
               USA, January 7-12, 2008},
  pages     = {273--284},
  publisher = {{ACM}},
  year      = {2008},
  _url       = {http://doi.acm.org/10.1145/1328438.1328472},
  _doi       = {10.1145/1328438.1328472},
  timestamp = {Tue, 22 May 2012 15:24:56 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/popl/HondaYC08},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}
@incollection{DBLP:books/el/leeuwen90/Thomas90,
  author    = {Wolfgang Thomas},
  title     = {Automata on Infinite Objects},
  booktitle = {Handbook of Theoretical Computer Science, Volume {B:} Formal Models
               and Sematics {(B)}},
  publisher = {MIT Press},
  pages     = {133--192},
  year      = {1990},
  timestamp = {Thu, 03 Jan 2002 11:51:18 +0100},
  biburl    = {http://dblp.uni-trier.de/rec/bib/books/el/leeuwen90/Thomas90},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@inproceedings{DBLP:conf/lata/RotBR13,
  author    = {Jurriaan Rot and
               Marcello M. Bonsangue and
               Jan J. M. M. Rutten},
  editor    = {Adrian Horia Dediu and
               Carlos Mart{\'{\i}}n{-}Vide and
               Bianca Truthe},
  title     = {Coinductive Proof Techniques for Language Equivalence},
  booktitle = {Language and Automata Theory and Applications - 7th International
               Conference, {LATA} 2013, Bilbao, Spain, April 2-5, 2013. Proceedings},
  series    = {Lecture Notes in Computer Science},
  volume    = {7810},
  pages     = {480--492},
  publisher = {Springer},
  year      = {2013},
  ignoreurl       = {http://dx.doi.org/10.1007/978-3-642-37064-9_42},
  ignoredoi       = {10.1007/978-3-642-37064-9_42},
  timestamp = {Fri, 31 May 2013 12:37:26 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/conf/lata/RotBR13},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}
@book{sangiorgi2014introduction,
  title={An Introduction to Bisimulation and Coinduction},
  author={Sangiorgi, Davide},
  isbn={9781139161381},
  ignoreurl={https://books.google.de/books?id=PIjWoQEACAAJ},
  year={2014},
  publisher={Cambridge University Press}
}
@Article{Niwinski1984,
  author = 	 {Damian Niwinski},
  title = 	 {Fixed-Point Characterization of Context-Free $\infty$-Languages},
  journal = 	 {Information and Control},
  year = 	 1984,
  volume = 	 61,
  pages = 	 {247-276}
}

@article{DBLP:journals/iandc/ChristensenHS95,
  author    = {S{\o}ren Christensen and
               Hans H{\"{u}}ttel and
               Colin Stirling},
  title     = {Bisimulation Equivalence is Decidable for All Context-Free Processes},
  journal   = {Inf. Comput.},
  volume    = {121},
  number    = {2},
  pages     = {143--148},
  year      = {1995},
  ignoreurl       = {http://dx.doi.org/10.1006/inco.1995.1129},
  ignoredoi       = {10.1006/inco.1995.1129},
  timestamp = {Wed, 06 Jul 2011 21:24:02 +0200},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/iandc/ChristensenHS95},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}

@article{DBLP:journals/jacm/BaetenBK93,
  author    = {Jos C. M. Baeten and
               Jan A. Bergstra and
               Jan Willem Klop},
  title     = {Decidability of Bisimulation Equivalence for Processes Generating
               Context-Free Languages},
  journal   = {J. {ACM}},
  volume    = {40},
  number    = {3},
  pages     = {653--682},
  year      = {1993},
  ignoreurl       = {http://doi.acm.org/10.1145/174130.174141},
  ignoredoi       = {10.1145/174130.174141},
  timestamp = {Thu, 20 Nov 2003 12:28:00 +0100},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/jacm/BaetenBK93},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}
@article{DBLP:journals/jacm/AcetoH92,
  author    = {Luca Aceto and
               Matthew Hennessy},
  title     = {Termination, Deadlock, and Divergence},
  journal   = {J. {ACM}},
  volume    = {39},
  number    = {1},
  pages     = {147--187},
  year      = {1992},
  ignoreurl       = {http://doi.acm.org/10.1145/147508.147527},
  ignoredoi       = {10.1145/147508.147527},
  timestamp = {Thu, 20 Nov 2003 12:27:59 +0100},
  biburl    = {http://dblp.uni-trier.de/rec/bib/journals/jacm/AcetoH92},
  bibsource = {dblp computer science bibliography, http://dblp.org}
}
@inproceedings{DBLP:conf/esop/ToninhoCP13,
  author    = {Bernardo Toninho and
               Lu{\'{\i}}s Caires and
               Frank Pfenning},
  title     = {Higher-Order Processes, Functions, and Sessions: {A} Monadic Integration},
  booktitle = {{ESOP}},
  _series    = {Lecture Notes in Computer Science},
 series    = {LNCS},
  volume    = {7792},
  pages     = {350--369},
  publisher = {Springer},
  year      = {2013}
}

@Article{ huttel.lanese.etal:foundations-session-types,
  author = 	 {Hans Hüttel and
Ivan Lanese and
Vasco T. Vasconcelos and
Luís Caires and
Marco Carbone and
Pierre-MALO Deniélou and
Dimitris Mostrous and
Luca Padovani and
António Ravara and
Emilio Tuosto and
Hugo Torres Vieira and
Gianluigi Zavattaro},
  title = 	 {Foundations of Session Types and Behavioural Contracts},
  journal = 	 {ACM Comput.\ Surv.},
  year = 	 {2016},
  volume = 	 {49},
  number = 	 {1},
  OPTpages = 	 {},
  OPTmonth = 	 {June},
  OPTnote = 	 {},
  OPTannote = 	 {}
}

@Book{Pierce2002-tpl,
  author =	 {Pierce, Benjamin C.},
  title = 	 {Types and Programming Languages},
  publisher = 	 {MIT Press},
  year = 	 2002
}

@article{DBLP:journals/iandc/LanesePSS11,
  author    = {Ivan Lanese and
               Jorge A. P{\'{e}}rez and
               Davide Sangiorgi and
               Alan Schmitt},
  title     = {On the expressiveness and decidability of higher-order process calculi},
  journal   = {Inf. Comput.},
  volume    = {209},
  number    = {2},
  pages     = {198--226},
  year      = {2011}
}
                                                                                                                                         doc/ICFP2016/arithmetic-server-sepi.cfs                                                             0000664 0001750 0001750 00000001106 13216017506 020024  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               def client c: TermChannel;?int =
  c select Add.
  c select Const. c!5
  c select Mult.
  c select Const. c!7
  c select Const. c!9
  c?n. printInteger!n

def receiveEval (c: forall alpha.dualof TermChannel;alpha, r:!int) =
  case c of
    Const ->
      c?n.r!n
    Add ->
      new r1 s1:!int receiveEval!(c,r1).s1?n1.
      new r2 s2:!int receiveEval!(c,r2).s2?n2.
      r!(n1+n2)
    Mult ->
      -- as above
    r!(n1*n2)

def computeService c: dualof TermChannel;!int =
  new rs: !int
  receiveEval!(c,r). s?n.c!n

-- main
new cs:TermChannel;?int
client!c |
computeService!s
                                                                                                                                                                                                                                                                                                                                                                                                                                                          doc/ICFP2016/fig-lts-bpa.tex                                                                        0000664 0001750 0001750 00000001401 13216017506 015561  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[tp]
  \begin{gather*}
    \DONE{\varepsilon} \qquad
    \frac{\DONE{E_1}\quad \DONE{E_2}}{\DONE{E_1;E_2}} \qquad
    \frac{\DONE{E}}{\DONE{x}}~x=E \in\Xi
    \\
    {a \LTSderives \varepsilon }
    \qquad
    \frac{E \LTSderives E'}{x \LTSderives E'}
    ~x=E \in \Xi
    \\
    \frac{E_1 \LTSderives E_1'}{E_1 + E_2 \LTSderives E_1'}
    \qquad
    \frac{E_2 \LTSderives E_2'}{E_1 + E_2 \LTSderives E_2'}
    \\
    \frac{E_1 \LTSderives E_1'}{E_1; E_2 \LTSderives E_1';E_2}
    \qquad
    \frac{\DONE{E_1} \quad E_2 \LTSderives E_2'}{E_1; E_2 \LTSderives
      E_2}
  \end{gather*}
  \caption{Labelled transition system for basic process algebra}
  \label{fig:lts-bpa}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                               doc/ICFP2016/sigplanconf-template.tex                                                               0000664 0001750 0001750 00000010203 13216017506 017570  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               %-----------------------------------------------------------------------------
%
%               Template for sigplanconf LaTeX Class
%
% Name:         sigplanconf-template.tex
%
% Purpose:      A template for sigplanconf.cls, which is a LaTeX 2e class
%               file for SIGPLAN conference proceedings.
%
% Guide:        Refer to "Author's Guide to the ACM SIGPLAN Class,"
%               sigplanconf-guide.pdf
%
% Author:       Paul C. Anagnostopoulos
%               Windfall Software
%               978 371-2316
%               paul@windfall.com
%
% Created:      15 February 2005
%
%-----------------------------------------------------------------------------


\documentclass{sigplanconf}

% The following \documentclass options may be useful:

% preprint      Remove this option only once the paper is in final form.
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

\usepackage{amsmath}


\begin{document}

\special{papersize=8.5in,11in}
\setlength{\pdfpageheight}{\paperheight}
\setlength{\pdfpagewidth}{\paperwidth}

\conferenceinfo{CONF 'yy}{Month d--d, 20yy, City, ST, Country} 
\copyrightyear{20yy} 
\copyrightdata{978-1-nnnn-nnnn-n/yy/mm} 
\doi{nnnnnnn.nnnnnnn}

% Uncomment one of the following two, if you are not going for the 
% traditional copyright transfer agreement.

%\exclusivelicense                % ACM gets exclusive license to publish, 
                                  % you retain copyright

%\permissiontopublish             % ACM gets nonexclusive license to publish
                                  % (paid open-access papers, 
                                  % short abstracts)

\titlebanner{banner above paper title}        % These are ignored unless
\preprintfooter{short description of paper}   % 'preprint' option specified.

\title{Title Text}
\subtitle{Subtitle Text, if any}

\authorinfo{Name1}
           {Affiliation1}
           {Email1}
\authorinfo{Name2\and Name3}
           {Affiliation2/3}
           {Email2/3}

\maketitle

\begin{abstract}
This is the text of the abstract.
\end{abstract}

\category{CR-number}{subcategory}{third-level}

% general terms are not compulsory anymore, 
% you may leave them out
\terms
term1, term2

\keywords
keyword1, keyword2

\section{Introduction}

The text of the paper begins here.

Lots of text.

More text.

Lots of text.

More text.


Lots of text.

More text.

Lots of text.

More text.


Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.


Lots of text.

More text.

Lots of text.

More text.


Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.


Lots of text.

More text.

Lots of text.

More text.




Lots of text.

More text.

Lots of text.

More text.

Lots of text.


Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

More text.

Lots of text.

\appendix
\section{Appendix Title}

This is the text of the appendix, if you need one.

\acks

Acknowledgments, if needed.

% We recommend abbrvnat bibliography style.

\bibliographystyle{abbrvnat}

% The bibliography should be embedded for final submission.

\begin{thebibliography}{}
\softraggedright

\bibitem[Smith et~al.(2009)Smith, Jones]{smith02}
P. Q. Smith, and X. Y. Jones. ...reference text...

\end{thebibliography}


\end{document}

%                       Revision History
%                       -------- -------
%  Date         Person  Ver.    Change
%  ----         ------  ----    ------

%  2013.06.29   TU      0.1--4  comments on permission/copyright notices

                                                                                                                                                                                                                                                                                                                                                                                             doc/ICFP2016/proofs-type-equivalence.tex                                                            0000664 0001750 0001750 00000016627 13216017506 020262  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \newpage
\subsection{Properties of session type equivalence}

\subsubsection{Contractivity}
\label{sec:contractivity}


Consider the (illformed) type $W = \mu x.(\skipk; x)$ and the rule
\begin{equation}
  F(R) = \dots {}\cup{} \{ ((\skipk;S_1), S_2) \mid (S_1, S_2) \in R \}\label{eq:1}
\end{equation}
Because the set $R = \{(W,T), ((\skipk; W), T) \mid \cdot \vdash T :: S\}$ is $F$-consistent, every session type would
be equivalent to $W$! With symmetry and transitivity, every session type would be equivalent to
every other.

Fortunately, contractivity rules out types like $W$ so that the $\skipk$-canceling rules like~\eqref{eq:1} can only be
applied a finite number of times between non-skip rules.

\begin{lemma}\label{lemma:S=skip}
  If $S \approx \skipk$, then $S \grmeq \skipk \grmor (S; S) \grmor
  \mu x.S$.
\end{lemma}
\begin{proof}
  The proof rests on the observation that the derivation of $S \approx
  \skipk$ is finite because of contractivity. Thus, induction on the
  derivation yields the result.
\end{proof}

\begin{lemma}\label{lemma:trans-skip}
  If $S_1 \approx \skipk$ and $\skipk \approx S_3$, then $S_1 \approx S_3$.
\end{lemma}
\begin{proof}
  Construct a derivation for $S_1\approx S_3$ by induction on $S_1$
  using its structure according to Lemma~\ref{lemma:S=skip}.

  \textbf{Case $\skipk$.} Immediate by assumption.

  \textbf{Case $\mu x.S_1'$.} By  Lemma~\ref{lemma:S=skip}, $S_1'[\mu
  x.S_1'/x] = S_1'$ so inversion of the $\mu$-left rule yields $S_1'
  \approx \skipk$. By induction, there is a derivation for $S_1'
  \approx S_3$ which can be completed by the $\mu$-left rule to $S_1
  \approx S_3$.

  \textbf{Case $(S_1';S_1'')$ where $S_1' \approx \skipk$ and $S_1''
    \approx \skipk$.} We need to perform an auxiliary induction on
  $S_3$.

  %% This proof step requires the more general skip rules.
  \textbf{Subcase $\skipk$.} Apply rule skip-left-left.

  \textbf{Subcase $(S_3'; S_3'')$ where $\skipk \approx S_3'$ and
    $\skipk \approx S_3''$.} Apply the semicolon rule to the
  inductively constructed proofs for $S_1' \approx S_3'$ and $S_1''
  \approx S_3''$.

  \textbf{Subcase $\mu x. S_3'$.} Use the $\mu$-right rule. Analogous
  to the case for the $\mu$-left rule.
\end{proof}

\begin{lemma}\label{lemma:S=B}
  If $S \approx {!B}$, then $S \grmeq {!B} \grmor (S^{\skipk};
  S)\grmor (S; S^{\skipk}) \grmor \mu x.S$ where $S^{\skipk}$ is
  described in Lemma~\ref{lemma:S=skip}.
\end{lemma}

\begin{lemma}
  If $S_1 \approx {!B}$ and ${!B} \approx S_3$, then $S_1 \approx S_3$.  
\end{lemma}
\begin{proof}
  Construct a derivation for $S_1 \approx S_3$ by induction on $S_1$
  using its structure according to Lemma~\ref{lemma:S=B}.

  \textbf{Case $!B$.} Immediate by assumption.

  \textbf{Case $(S^{\skipk}; S)$ where $S^{\skipk}\approx \skipk$ and
    $S \approx{!B}$.} By induction $S \approx S_3$. Applying rule
  skip-l-l yields $S_1 \approx S_3$.

  \textbf{Case $(S; S^{\skipk})$ where $S^{\skipk}\approx \skipk$ and
    $S \approx{!B}$.} Similar.

  \textbf{Case $\mu x.S_1'$.} By Lemma~\ref{lemma:S=B}, $S_1'[\mu
  x.S_1'/x] = S_1'$, so inversion of the $\mu$-left rule yields $S_1'
  \approx {!B}$. By induction, there is a derivation for $S_1'
  \approx S_3$ which can be completed by the $\mu$-left rule to $S_1
  \approx S_3$.
\end{proof}

\begin{lemma}\label{lemma:S=oplus}
  If $S \approx {\oplus\{l_i\colon S_i\}_{i\in I}}$, then $S \grmeq {\oplus\{l_i\colon S_i'\}_{i\in I}} \grmor (S^{\skipk};
  S)\grmor (S; S^{\skipk}) \grmor \mu x.S$ where $S_i \approx S_i'$ and  $S^{\skipk}$ is
  described in Lemma~\ref{lemma:S=skip}.
\end{lemma}

\begin{lemma}
  If $S_1 \approx {\oplus\{l_i\colon S_i\}_{i\in I}}$ and
  ${\oplus\{l_i\colon S_i\}_{i\in I}} \approx S_3$, then $S_1 \approx
  S_3$.
\end{lemma}

\subsubsection{Reflexivity}
\label{sec:reflexivity}

Let $R = \{ (T, T) \mid \cdot \vdash T :: \kinds \} \cup \{(T'[\mu
x.T' / x], \mu x.T') \mid \cdot \vdash T' :: \kinds \}$. Show that $R \subseteq F(R)$.

Obvious, except for $(\mu x.T', \mu x.T') \in R$, but in this case
$(\mu x.T', \mu x.T') \in F (\{(T'[\mu x.T' / x], \mu x.T')\}) \subseteq F (R)$.

For $(T'[\mu x.T' / x], \mu x.T') \in R$, observe that
$$(T'[\mu x.T' / x], \mu x.T') \in F (\{(T'[\mu x.T' / x], T'[\mu x.T'/x])\}) \subseteq F (R).$$

\subsubsection{Symmetry}
\label{sec:symmetry}

Let $R = \{ (T_2, T_1) \mid T_1 \approx T_2 \}$. Show that $R \subseteq F(R)$.

To consider an exemplary case,
suppose that $((S_1'; S_2'), (S_1; S_2)) \in R$ because $((S_1; S_2) \approx (S_1'; S_2'))$ because
$(S_1 \approx S_1')$ and $(S_2 \approx S_2')$. From the latter two assumptions, we obtain that
$(S_1', S_1) \in R$ and $(S_2', S_2) \in R$, so that $((S_1'; S_2'), (S_1; S_2)) \in F(R)$.

Suppose that $(S', \mu x.S) \in R$ because $\mu x.S \approx S'$ because $S[\mu x.S/x] \approx
S'$. From the latter assumption, we obtain that $(S', S[\mu x.S/x]) \in R$ and hence $(S', \mu x.S)
\in F (R)$. The other case involving $\mu$ is analogous.

\subsubsection{Transitivity}
\label{sec:transitivity}

Let $R = \{ (T, T'') \mid \exists T'. T \approx T' \wedge T' \approx T''\}$. 
Show that $R \subseteq F (R)$.

Observe that, by reflexivity of $\approx$, ${\approx} \subseteq R$.

Suppose that $(\mu x. S, T) \in R$ because $(\mu x.S, T) \in
{\approx}$ and $(T, T) \in {\approx}$ (by reflexivity). Now  $(\mu x.S, T) \in
{\approx}$ because $(S[\mu x.S/x], T) \in {\approx}$ and hence $(S[\mu
x.S/x], T) \in R$, so that $(\mu x. S, T) \in F(R)$. 

More cases omitted.

Consider $(S_1, (\skipk; S_2)) \in {\approx}$ (because  $S_1\approx S_2$)
and $((\skipk; S_2), S_3) \in {\approx}$ (because $S_2 \approx S_3$).
Hence $(S_1, S_3) \in R$. Must show that $(S_1, S_3) \in F (R)$!

But it might be the case that $S_2 = (\skipk; S_2')$ so that, by inverting the same rule, $S_1
\approx S_2'$  and $S_2' \approx S_3$. Fortunately, contractivity tells us that $S_2$ cannot contain 
unguarded recursion, so that an auxiliary induction on $S_2$ or on the proof of contractivity of $S_2$ can be
applied. 

\textbf{Case $S_2 = \skipk$.} In this case, we have $S_1 \approx
\skipk$ and $\skipk \approx S_3$ and by Lemma~\ref{lemma:trans-skip} we
obtain $(S_1, S_3) \in {\approx} = F ({\approx}) \subseteq F (R)$. 

% \textbf{Subcase $S_1 = \skipk$, $S_3 = \skipk$.} Trivally in $F(R)$.

% \textbf{Subcase $S_1 = \skipk$, $S_3 = \mu x.S'$.} It must be that $\skipk \approx S'[\mu x.S'/x]$,
% so $(\skipk, S'[\mu x.S'/x]) \in R$, so $(\skipk, \mu x.S') \in F(R)$.

% \textbf{Subcase $S_1 = \skipk$, $S_3 = (\skipk; S_3')$.} It must be that $(\skipk, S_3') \in
% {\approx} \subseteq R$, so $(\skipk, S_3) \in F (R)$.

% \textbf{Subcase $S_1 = \skipk$, $S_3 = (S_3'; \skipk)$.} Similar.

% \textbf{Subcase $S_1 = \skipk$, $S_3 = ((S_2';S_3'); S_4')$.} It must be that $(\skipk,
% (S_2';(S_3'; S_4'))) \in {\approx} \subseteq R$, so $(\skipk, ((S_2';S_3'); S_4')) \in F(R)$.

% \textbf{Subcase $S_1 = \skipk$, $S_3 = (S_2';(S_3'; S_4'))$.} Similar.

% \textbf{Subcase $S_1 = \mu x.S_1'$, $S_3 = \mu x.S_3'$.}
% It must be that $S_1'[\mu x.S_1'/x] \approx \skipk$ and
% $\skipk \approx S_3'[\mu x.S_3'/x]$.
% By Lemma~\ref{lemma:S=skip}, $x$ does not occur in $S_1'$ or $S_3'$ so
% we have $S_1' \approx \skipk$ and $\skipk \approx S_3'$.

\textbf{Case $S_2 = !B$.} 
In this case, we have $S_1 \approx {!B}$ and ${!B} \approx S_3$ and by
Lemma~\ref{lemma:S=B} we obtain  $(S_1, S_3) \in {\approx} = F
({\approx}) \subseteq F (R)$.

\textbf{Case $S_2 = ?B$.} Analogous.

\newpage
%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                         doc/ICFP2016/macros.tex                                                                             0000664 0001750 0001750 00000017176 13216017506 014760  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               % notes
\usepackage{xcolor}
\newcommand{\todo}[2]{[{\color{blue}\textbf{#1}}: {#2}]}
\newcommand{\vv}[1]{\todo{VV}{#1}}
\newcommand{\pt}[1]{\todo{PT}{#1}}

% Relations, predicates and operators
%\newcommand\PAR{|}
\newcommand\PAR{\mid}
\newcommand\DONE[1]{#1 \checkmark}
\newcommand\PDA{\mathcal{P}}
\newcommand\SEnv{\sigma} % syntactic environment
\newcommand\LEnv{\delta} % language environment
\newcommand\bisim\sim
\newcommand\Power{\wp}
\newcommand{\Silent}{\tau}
\newcommand{\Wderives}[1][{}]{\stackrel{#1}{\Longrightarrow}}
\newcommand{\LTSderives}[1][a]{\stackrel{#1}{\longrightarrow}}
\newcommand{\BPAderives}[1][a]{\stackrel{#1}{\longrightarrow}}
\newcommand{\reduces}{\rightarrow}
\newcommand{\subs}[2]{[{#1}/{#2}]}
\newcommand\Rangeof[1]{\llparenthesis#1\rrparenthesis}
\newcommand{\GFP}{\mathbf{gfp}}
\newcommand{\subj}{\mathsf{subj}}
\newcommand{\agree}{\mathsf{agree}}
\newcommand{\un}{\mathsf{un}}
\newcommand{\lin}{\mathsf{lin}}
\newcommand{\dual}[1]{\overline{#1}}
\newcommand{\dualof}{\mathsf{dual}}
\newcommand\Contr{\vdash_{c}}
%\newcommand\Contr{\vdash}
\newcommand\Guarded{\mathsf{s}} % Should this be Skips?
\newcommand\Productive{\mathsf{p}}
\newcommand\Embed[1]{{(#1)}^\dagger}
\newcommand\Unfold{\mathsf{unf}} % shortened from "unfold"
\newcommand\Unravel{\mathsf{unr}} % shortened from "unravel"
\newcommand\Norm{\mathsf{norm}}
\newcommand\NT{\mathcal{NT}} % nonterminals
%\newcommand\Tyvars{\mathcal{TV}} % not used (vv)
%\newcommand{\Labels}{\mathcal L} % The set of labels in choice types; not used (vv)
%\newcommand{\btypes}{\mathcal B} % The set of base types; not used (vv)
\newcommand{\stypes}{\mathcal S} % The set of closed session types
\newcommand{\types}{\mathcal T} % the set of (well formed) types
\newcommand{\subterms}{\mathsf{sub}}
\newcommand{\cardinality}[1]{|{#1}|}
\newcommand{\measure}{\mathcal M}
%\newcommand\TypeSim{\preceq}
\newcommand\TypeEquiv{\sim}
\newcommand{\unguarded}{\mathsf{unguarded}}
\newcommand\TL{\ensuremath{\mathcal U}} %trace language
\newcommand{\TR}{\mathsf{TR}}
\newcommand{\TRw}{\mathsf{TR}^\omega}
\newcommand\toBPATop[1]{\mathsf{BPATop} (#1)}
\newcommand\toBPA[1]{\mathsf{BPA} (#1)}
\newcommand\toCFG{\mathsf{CFG}}
\newcommand\toLHS{\mathsf{RHS}}
\newcommand\BLANK{\$}
%\newcommand\Nat{\mathbf{N}} % was: used once, never defined
\newcommand\MACHINE{\mathcal{M}}
\newcommand\RHS{\mathsf{rhs}}
\newcommand\Return{\ensuremath{\mathsf{return}}}
\newcommand\Do{\ensuremath{\mathsf{do}}}
\newcommand\Out{\ensuremath{\mathsf{out}}}
\newcommand\Fresh{\ensuremath{\mathsf{fresh}}}
\newcommand{\eqdef}{\triangleq} % equal by definition

% syntax variables
\newcommand\prekind{\upsilon}
\newcommand\kind{\kappa}
\newcommand\kinds{\stypes}
\newcommand\kindt{\types}
%\newcommand\kindsch{\Box}
\newcommand\kindsch{\mathcal C}
\newcommand{\isScheme}{~\scheme} % mathsf is always \upshape and slighter smaller
%\newcommand{\isOk}{~\mathsf{ok}}
\newcommand{\isOk}{:\mathsf{type}}
\newcommand\Unrestricted{\ensuremath{\mathbf{u}}} % \infty
\newcommand\Linear{\ensuremath{\mathbf{l}}} % 1 
%\newcommand\GEnv{\Theta}
\newcommand\GEnv{\Delta}
\newcommand\BPAprocess\Theta

% Labels for branches and choices
\newcommand\lbl{\textit}

% Keywords
\newcommand{\keyword}[1]{\mathsf{#1}}
\newcommand{\skipk}{\keyword{skip}}
\newcommand{\intk}{\keyword{int}}
\newcommand{\unitk}{\keyword{unit}}
\newcommand{\sendk}{\keyword{send}}
\newcommand{\recvk}{\keyword{receive}}
\newcommand{\newk}{\keyword{new}}
\newcommand{\forkk}{\keyword{fork}}
% \newcommand{\inlk}{\keyword{inl}} % Are we going for labelled injection?
% \newcommand{\inrk}{\keyword{inr}}
\newcommand{\fixk}{\keyword{fix}}
\newcommand{\letk}{\keyword{let}}
\newcommand{\ink}{\keyword{in}}
\newcommand{\matchk}{\keyword{match}}
\newcommand{\withk}{\keyword{with}}
\newcommand{\selectk}{\keyword{select}}
\newcommand{\casek}{\keyword{case}}
\newcommand{\ofk}{\keyword{of}}
\newcommand{\scheme}{\keyword{sch}}
\newcommand{\End}{\keyword{end}}

% Process constructors
\newcommand{\send}[2]{\sendk\,{#1}\,{#2}}
\newcommand{\recv}[1]{\recvk\,{#1}}
\newcommand{\fork}[1]{\forkk\,{#1}}
\newcommand{\new}[1]{\nu\,{#1}}
\newcommand{\letin}[3]{\letk\,{#1}={#2}\,\ink\,{#3}}
\newcommand{\match}[2]{\matchk\,{#1}\,\withk\,[{#2}]}
\newcommand{\fix}[2]{\fixk\,{#1}.{#2}}
\newcommand{\select}[2]{\selectk{\,{#1}\,{#2}}}
\newcommand{\inject}[2]{\ink{\,{#1}\,{#2}}}
\newcommand{\case}[2]{\casek\,{#1}\,\ofk\,{#2}}

% Grammars
\newcommand{\grmeq}{\; ::= \;}
\newcommand{\grmor}{\;\mid\;}

% Theorem
\newtheorem{theorem}{Theorem}[section]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{corollary}[theorem]{Corollary}
\newtheorem{conjecture}[theorem]{Conjecture}

\theoremstyle{definition}
\newtheorem{definition}{Definition}[section]

% Listings

% Programming language: cfs
\lstdefinelanguage{cfs}{
  language=haskell,
%  style=eclipse
  basicstyle=\sffamily\small,
  extendedchars=true,
  breaklines=true,
  morekeywords={new,send,receive,skip,select,dualof,int,fork,def},
  tabsize=8,
  literate=
    {oplus}{$\oplus$}1 
    {otimes}{$\otimes$}1 
    {forall}{$\forall$}1
    {alpha}{$\alpha$}1 
    {beta}{$\beta$}1 
    {->}{$\rightarrow$}1 
    {==}{$\equiv$}1 
    {lambda}{$\lambda$}1 
    {->}{$\rightarrow$}1 
    {-o}{$\multimap$}1
}

\lstset{language=cfs}   % Default language 

%%% imported macros (Peter)

\newcommand\kw\keyword %keyword
\newcommand\MF[1]{\textup{\textit{#1}}} %metafunction
%
% general
\newcommand\Multi[1]{\overrightarrow{#1}}

% syntax types
% meta variables
\newcommand\TY{T}               %type
\newcommand\TG{G}               %ground type
\newcommand\SE{S}               %session
\newcommand\LL{l}               %label
\newcommand\GTY{G}              %ground type
\newcommand\CG{\textit{ls}}     %choice group
\newcommand\TC{\textit{tc}}     %type constructors
\newcommand\TE{\Gamma}          %type environments
\newcommand\TEempty{\cdot}
% choice group constructors
\newcommand\cgNothing\cdot
% \newcommand\cgAlt[2]{#1:#2}     %in sessions
% \newcommand\altSingle[2]{#1:#2} %in expressions
\newcommand\cgAlt[2]{#1\colon #2}     %in sessions % \colon gives
                                %better spacing
\newcommand\altSingle[2]{#1\colon #2} %in expressions
% session constructors
%\newcommand\sEnd{\kw{END}} % use \End
\newcommand\sRecv[1]{\textup{?}\,#1.}
\newcommand\sSend[1]{\textup{!}\,#1.}
\newcommand\sRecvChoice[2][{}]{\&^{#1}\{#2\}}
\newcommand\sSendChoice[2][{}]{\oplus^{#1}\{#2\}}
\newcommand\sVar{z}%{\sigma}
\newcommand\sMu[1]{\mu#1.}

\newcommand\scEnd{\kw{END}}
\newcommand\scRecvChoice[2][{}]{\&^{#1}\{#2\}}
\newcommand\scSendChoice[2][{}]{\oplus^{#1}\{#2\}}
\newcommand\scCompose[1]{#1{;}}%{#1;\,}
\newcommand\scUnit{\kw{skip}}
\newcommand\scVar{z}
\newcommand\scMu[1]{\mu#1.}


\newcommand\tNProd[2][{}]{\prod^{#1}\langle#2\rangle}
\newcommand\tNSum[2][{}]{\sum^{#1}\langle#2\rangle}


\newcommand\scRecv{\textup{?}}
\newcommand\scSend{\textup{!}}


% type constructors
\newcommand\tcUnit{\unitk}%{*}
\newcommand\tcBase{B}
\newcommand\tcPair\times
\newcommand\tcSum{+}
\newcommand\tcLolli\multimap
\newcommand\tcFun\to
\newcommand\tcPort[1]{{[#1]}}
\newcommand\tcBang{\mathop!}
% types
\newcommand\tUnit\tcUnit
\newcommand\tBase\tcBase
\newcommand\tPair[2]{#1\tcPair#2}
\newcommand\tSum[2]{#1\tcSum#2}
\newcommand\tLolli[2]{#1\tcLolli#2}
\newcommand\tFun[2]{#1\tcFun#2}
\newcommand\tPort[1]{{[#1]}}
\newcommand\tBang[1]{\tcBang#1}
\newcommand\tDyn{D}
\newcommand\tCoerce[3][{}]{#2\stackrel{#1}{\Rightarrow} #3}
\newcommand\tWild{\diamondsuit}
\newcommand\tCrash{\spadesuit}
\newcommand\tVar{z}%{X}%{\tau}
\newcommand\tMu[1]{\mu#1.}

%metafunctions
\newcommand\dom{\mathsf{dom}}
\newcommand\seq{\mathsf{seq}}
\newcommand\Free{\mathsf{free}}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                                                  doc/ICFP2016/fig-algorithmic-type-equiv.tex                                                         0000664 0001750 0001750 00000002644 13216017506 020641  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[t]
  \begin{gather*}
    \frac{
      S_1 \equiv S_2 \in \Sigma
    }{
      \Sigma \vdash S_1 \equiv S_2
    }
    \quad
    \frac{}{
      \Sigma \vdash \skipk \equiv \skipk
    }
    \\
    \frac{
      \Sigma \vdash S_1 \equiv S_2
    }{
      \Sigma \vdash \alpha;S_1 \equiv \alpha;S_2
    }
    \;\,
    \frac{
      \Sigma \vdash S_1 \equiv S_2
    }{
      \Sigma \vdash \;!B;S_1 \equiv \;!B;S_2
    }
    \;\,
    \frac{
      \Sigma \vdash S_1 \equiv S_2
    }{
      \Sigma \vdash \;?B;S_1 \equiv \;?B;S_2
    }
    \\
    \frac{
      \Sigma \vdash S_i \equiv S_i'
      \quad
      \forall i \in I
    }{
      \Sigma \vdash \oplus\{l_i\colon S_i\}_{i\in I} \equiv \oplus\{l_i\colon S'_i\}_{i\in I} 
    }
    \\
    \frac{
      \Sigma \vdash S_i \equiv S_i'
      \quad
      \forall i \in I
    }{
      \Sigma \vdash \&\{l_i\colon S_i\}_{i\in I} \equiv \&\{l_i\colon S'_i\}_{i\in I} 
    }
    \\
    \frac{
      \Sigma, S_1 \equiv S_2 \vdash \Unfold(S_1) \equiv S_2
      \quad
      S_1\,\unguarded
    }{
      \Sigma \vdash S_1 \equiv S_2
    }
    \\
    \frac{
      \Sigma, S_1 \equiv S_2 \vdash S_1 \equiv \Unfold(S_2)
      \quad
      S_2\,\unguarded
    }{
      \Sigma \vdash S_1 \equiv S_2
    }
  \end{gather*}
  Rules should be tried in order
  \caption{Algorithmic type equivalence}
  \label{fig:alg-type-equiv}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                            doc/ICFP2016/intro.tex                                                                              0000664 0001750 0001750 00000026740 13216017506 014624  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               
\section{Introduction}
\label{sec:introduction}

Session types have been discovered by Kohei Honda as a means to describe the structured
interaction of processes via typed communication channels
\cite{DBLP:conf/concur/Honda93,DBLP:conf/parle/TakeuchiHK94}. While connections are homogeneously
typed in languages like Concurrent ML
\cite{DBLP:conf/pldi/Reppy91}, 
session types provide a heterogeneous type discipline for a
protocol on a bidirectional connection: each message has an individual direction
and type and there are choice points where a sender can make a choice and a receiver
has to follow. 
While session types have been conceived for process
calculi, they provide precise typings for communication
channels in any programming language. They fit particularly well with
strongly typed functional languages from the ML family.

The type structure of a functional language with session types
typically comes with two layers, regular types and session types:
\begin{align}
    \TY & ::=  \SE \mid \tUnit \mid \tBase \mid \tFun\TY\TY \mid \dots
    % \tNProd{\cgAlt\LL{\TY_\LL}} \mid \tNSum{\cgAlt\LL{\TY_\LL}} \mid
    %       \tVar \mid \tMu\tVar\TY
  \label{eq:3}\\\notag
    \SE & ::=  \End  \mid \sRecv\TY\SE \mid \sSend\TY\SE \mid
%    \sRecvChoice{\cgAlt{\LL_i}{\SE_i}}_{i\in I} \mid \sSendChoice{\cgAlt{\LL_i}{\SE_i}}_{i\in I} \mid 
    \sRecvChoice{\cgAlt{\LL_i}{\SE_i}} \mid \sSendChoice{\cgAlt{\LL_i}{\SE_i}} \mid 
    \sVar \mid \sMu\sVar\SE
\end{align}
A type $\TY$ is either a session type $\SE$, a unit type, a base type $\tBase$,
a function type, and so on. Session types $\SE$ are attached to
communication channels. They denote denote different states of the
channel. The type $\End$ indicates the end of a session,
$\sRecv\TY\SE$ ($\sSend\TY\SE$) indicates readiness to receive (send)
a value of type $\TY$ and continuing with $\SE$,  the branching
operators $\sRecvChoice{\cgAlt{\LL_i}{\SE_i}}_{i\in I}$ and
 $\sSendChoice{\cgAlt{\LL_i}{\SE_i}}_{i\in I}$ indicate receiving and sending
labels, where the label $\LL_i$ selects the 
protocol $\SE_i$ from a finite number of possibilities $i\in I$ for the
subsequent communication on the channel. For example, the session type
\begin{displaymath}
  \sRecvChoice{
    \cgAlt{\lbl{add}}{\sRecv{\intk}\sRecv{\intk}\sSend{\intk}\End}
    ,
    \cgAlt{\lbl{neg}}{\sRecv{\intk}\sSend{\intk}\End}
  }
\end{displaymath}
is the type of a server that accepts two commands \lbl{add} and \lbl{neg},
then reads the appropriate number of arguments and returns the result of the command.
The session variable $\sVar$ and the operator $\sMu\sVar\SE$ serve to
introduce recursive protocols, for example, to read a list of numbers:
\begin{displaymath}
  \sMu\sVar \sRecvChoice{
    \cgAlt{\lbl{stop}}\End
    ,
    \cgAlt{\lbl{more}}{\sRecv{\intk}\sVar}
  }
\end{displaymath}

Session types are well suited to document high-level communication
protocols and there is a whole range of extensions to make them
amenable to deal with realistic situations, for example, multi-party
session types~\cite{DBLP:conf/popl/HondaYC08}, session types for
distributed object-oriented
programming~\cite{DBLP:conf/popl/GayVRGC10}, or for programming web
services~\cite{DBLP:journals/toplas/CarboneHY12}. However, there is a
fundamental limitation in their structure that makes it impossible to
describe efficient low-level serialization (marshalling, pickling, \dots) of
tree structured data in a type-safe way, as we demonstrate with the
following example.

Let's assume that a single communication operation can only transmit a
label or a base type value to model the real-world restriction that
data structures need to be serialized to a wire format before they can
be sent over a network connection.  Formally, it is sufficient to
restrict the session type formation for sending and receiving data to
base types: $\sSend{B}\SE$ and $\sRecv{B}\SE$. Now suppose we want to
transmit binary trees where the internal nodes contain a number.  A
recursive type for such trees can be defined as follows:
\lstinputlisting{treedata.cfs}
% \begin{displaymath}
%   \mathit{Tree} = \tMu\tVar \mathit{Leaf} (\intk) + \mathit{Node} (\tPair\tVar\tVar)
% \end{displaymath}
To serialize such a structure, we traverse it in some order and transmit a sequence of
labels \lstinline|Leaf| and \lstinline|Node| and \lstinline|int|
values as they are visited by the traversal. The set of serialization
sequences corresponding to a pre-order traversal of a tree
may be described by the following context-free grammar.
\begin{equation}\label{eq:1}
  N ::= \mathsf{Leaf} \mid \mathsf{Node}\ \intk\ N\ N
\end{equation}
\begin{lstlisting}[float={t},captionpos={b},caption={Type-safe
    serialization of a binary tree},label={listing:serializing}]
sendTree : forallalpha.Tree -> TreeChannel;alpha -> alpha
sendTree Leaf c =
  select Leaf c
sendTree (Node x l r) c =
  let c1 = select Node c
      c2 = send x c1
      c3 = sendTree l c2
      c4 = sendTree r c3
  in  c4
\end{lstlisting}
Listing~\ref{listing:serializing}  contains a function \lstinline|sendTree| that
performs a pre-order traversal of a tree and sends correctly
serialized output on a channel.  The function relies on typical
operations in a functional session type calculus like
GV~\cite{DBLP:journals/jfp/GayV10}: the \lstinline|select| operation
takes a label and a channel, outputs the label, and returns the
(updated) channel. The \texttt{send} operation takes a value and a
channel, outputs the value, and returns the channel. Ignore the type
signature for a moment.

It turns out that the \lstinline|sendTree| function cannot be typed in existing
session-type calculi
\cite{DBLP:conf/concur/Honda93,DBLP:journals/acta/GayH05,DBLP:conf/esop/HondaVK98,DBLP:journals/jfp/GayV10,DBLP:conf/parle/TakeuchiHK94,DBLP:conf/esop/ToninhoCP13}. To
see this, we observe that the language generated by the nonterminal
$N$ in the grammar~\eqref{eq:1}
is context-free, but not regular.
In contrast, the language of communication actions described by a
traditional session type $S$ is regular. 
More precisely, taking infinite executions into account, 
each traditional session type is related to the union of a regular language and an
$\omega$-regular language that describe the finite and infinite sequences of communication actions
admitted by the type. A similar caveat applies to the language generated by a context-free grammar
like~\eqref{eq:1}, a point which we leave for future work. 

It turns out that we can type functions like \lstinline|sendTree| if we drop the
restriction of being tail recursive from the language of session
types. Here is an informal proposal for a revised session type structure
replacing the previous one~\eqref{eq:3}:
%
\begin{align*}
  \SE & ::=  \skipk \mid \scRecv\tBase \mid \scSend\tBase \mid
        \scCompose\SE\SE \mid
        \sRecvChoice{\cgAlt{\LL_i}{\SE_i}} \mid \sSendChoice{\cgAlt{\LL_i}{\SE_i}} \mid 
        \sVar \mid \sMu\sVar\SE
\end{align*}
%
That is, we remove the continuation from the primitive send and
receive types and adopt a general sequence operator $\scCompose\_\_$
with unit $\skipk$. This change removes the restriction to tail
recursion and enables a session type to express context-free
communication sequences such as the ones required for the
serialization example. However, the monoidal structure of
$\skipk$ and $\scCompose\_\_$ poses some challenges for the metatheory.  We
call this structure \emph{context-free session types} and it is
sufficient to assign a type to function \lstinline|sendTree|. First,
we define the recursive session type corresponding to the \lstinline|Tree|
datatype. Its definition follows the datatype definition, but
it makes the sequence of communication operations explicit.
\lstinputlisting{treechannel.cfs}
Now we are ready to explain the type signature for \lstinline|sendTree|.
\begin{lstlisting}
sendTree : forallalpha.Tree -> TreeChannel;alpha -> alpha
\end{lstlisting}
% \begin{displaymath}
%   \mathsf{sendTree} :
%   \begin{array}[t]{l}
%  \forall \alpha.
%   \mathsf{Tree} \\ \to
%   (\scMu\scVar
%   \scCompose{\scSendChoice{
%       \cgAlt{\kw{Leaf}}{\scSend\intk} \mid
%       \cgAlt{\kw{Node}}{(\scCompose\scVar\scVar)}}}\alpha) \\
%   \to \alpha
%   \end{array}
% \end{displaymath}
It abstracts over the type \lstinline|alpha| of the continuation
channel, takes as input a \lstinline|Tree| and a channel which first
runs the recursive  protocol \lstinline{TreeChannel} followed by some other
protocol specified by \lstinline|alpha|. The \lstinline|alpha|-typed
channel is returned which leaves its processing to the continuation.
%
This abstraction is required to make things work because a type of the form \lstinline|Tree ->  TreeChannel ->  skip|
does not fit the first recursive call.

Polymorphism, as seen in this signature, is rarely considered in session types
(with two exceptions \cite{BonoPadovaniTosatto13,DBLP:journals/mscs/Gay08} discussed in the related
work).  However, it appears quite natural in this context as sending a
tree generalizes sending a single value, which is naturally
polymorphic over the continuation channel as in $\kw{send} :\forall \alpha. \tBase \to
(\scCompose {\scSend\tBase}\alpha)
\to \alpha$.
% That is, the primitive \texttt{send} operation for a
% particular $\tBase$ could be given the type 
% \begin{displaymath}
%   \mathsf{send} : \forall \alpha.
%   \tBase \to
%   (\scCompose {\scSend\tBase}\alpha)
%   \to \alpha
%   \mathrm{.}
% \end{displaymath}
%
Further study of the typing derivation of \lstinline|sendTree| (in
Section~\ref{sec:context-free-session}) makes it clear that
polymorphism is absolutely essential to make context-free session
types work in connection with recursive types. It turns out that the
recursive calls happen at \emph{instances} of the declared type, so
that \lstinline|sendTree| (as well as its receiving counterpart,
\lstinline|recvTree|) makes use of \emph{polymorphic recursion}.

\subsection*{Contributions and overview}
\label{sec:contributions}

\begin{itemize}
\item We introduce context-free session types that extend the
  expressiveness of %traditional,
  regular
  session types to capture the type-safe serialization of recursive
  datatypes. They further enable the type-safe implementation
  of remote operations on recursive datatypes that either traverse the
  structure eagerly or on demand.
\item Section~\ref{sec:context-free-session} discusses the overall design and explains the
  requirements to the metatheory with examples.
\item Section~\ref{sec:types} formally introduces context-free session types. A kind
  system with subkinding guarantees well-formed\-ness; the definition of contractiveness needs to be
  refined to deal with the monoidal structure of the type operators $\skipk$ and $\_;\_$; we give a
  coinductive definition of type equivalence as a bisimulation of types and prove its decidability
  by reducing type equivalence to the equivalence of basic process
  algebra expressions (BPA).
\item Section~\ref{sec:processes} formally introduces the term
  language along with its statics and dynamics. It is a synchronous,
  first-order version of Gay and Vasconcelos' linear type theory for
  asynchronous session types (GV) \cite{DBLP:journals/jfp/GayV10}
  extended with recursive types and variant types to model recursive
  datatypes. We establish type soundness and progress for the
  sequential fragment of our language. We prove in
  Section~\ref{sec:conservative-extension} that our system
  conservatively extends a regular (first-order) session-type system.
\item We discuss related work in Section~\ref{sec:related-work} and conclude.
\end{itemize}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                doc/ICFP2016/main.tex                                                                               0000664 0001750 0001750 00000011616 13216017506 014411  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               %\documentclass[preprint]{sigplanconf}
\documentclass{sigplanconf}

% ICFP 2016:
% By Wednesday, March 16 2016, 15:00 (UTC), submit a full paper of at
% most 12 pages (6 pages for an Experience Report), in standard
% SIGPLAN conference format, including figures but excluding
% bibliography. 

% The following \documentclass options may be useful:

% preprint      Remove this option only once the paper is in final form.
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

\usepackage{amsmath,amssymb,amsthm}
\usepackage{stmaryrd}
\usepackage{listings,color}
\usepackage{alltt}
\usepackage{flushend}
\usepackage[utf8]{inputenc} % for proper diacritics: Universität, Hüttel, Luís
\usepackage[T1]{fontenc}

\input{macros}
%%% for the draft
% \renewcommand\vv[1]{}
% \renewcommand\pt[1]{}

\begin{document}
\toappear{}
% \special{papersize=8.5in,11in}
% \setlength{\pdfpageheight}{\paperheight}
% \setlength{\pdfpagewidth}{\paperwidth}

% \conferenceinfo{ICFP'16}{September 19--21, 2016, Nara, Japan} 
% \copyrightyear{2016} 
% \copyrightdata{978-1-nnnn-nnnn-n/yy/mm} 
% \doi{nnnnnnn.nnnnnnn}

% Uncomment one of the following two, if you are not going for the 
% traditional copyright transfer agreement.

%\exclusivelicense                % ACM gets exclusive license to publish, 
                                  % you retain copyright

%\permissiontopublish             % ACM gets nonexclusive license to publish
                                  % (paid open-access papers, 
                                  % short abstracts)

%\titlebanner{banner above paper title}        % These are ignored unless
%\preprintfooter{short description of paper}   % 'preprint' option specified.

\title{Context-Free Session Types}%Towards
%\subtitle{Subtitle Text, if any}

\authorinfo{Peter Thiemann}
           {Universität Freiburg, Germany}
           {thiemann@acm.org}
\authorinfo{Vasco T. Vasconcelos}
           {LaSIGE and University of Lisbon, Portugal}
           {vmvasconcelos@ciencias.ulisboa.pt}

\maketitle

\begin{abstract}
Session types describe structured communication on heterogeneously
typed channels at a high level.
Their tail-recursive structure imposes a protocol that can be
described by a regular language. 
The types of transmitted values are drawn from the underlying
functional language, abstracting from the
details of serializing values of structured data types.

Context-free session types extend session types by allowing nested
protocols that are not restricted to tail recursion. Nested protocols
correspond to deterministic context-free languages. Such protocols are
interesting in their own right, but they are particularly suited to
describe the low-level serialization of tree-structured data in a
type-safe way.  

We establish the metatheory of context-free session types, prove that
they properly generalize standard (two-party) session
types, and take first steps towards type checking by showing
that type equivalence is decidable.
\end{abstract}
% well we  (hope to) achieve the last two items

\category{D.3.3}{Language Constructs and Features}{Concurrent programming structures}
\category{D.3.1}{Formal Definitions and Theory}{}

% general terms are not compulsory anymore, 
% you may leave them out
% \terms
% term1, term2

\keywords
session types, semantics, type checking

\pagestyle{plain}

%  HOW DO WE ORGANISE THE PAPER?

% * 1 - 1.5 pages introduction, perhaps giving some small example already
% * >=2 pages motivation discussing a couple of progressively more complicated examples that highlight that problems and features
% --- do you think a running example is needed? I suppose that technical points can be explained with mini examples made on the spot or by reference to one of the examples from the motivation
% * Then defining formalities
% ** introducing types and type equiv
% ** expressions, statics, and dynamics
% * Then properties (perhaps better interleaved with the formalities)
% ** of types and equivalence
% ** type soundness
% * Then type checking
% ** specification
% ** decidability
% * Conservative extension
% * Related work
% * Conclusion

\input{intro}
\input{examples}
\input{types}
\input{processes}
\input{related}
\input{conclusion}

\acks % Acknowledgments, if needed.

The authors would like to thank Philip Wadler, Simon Gay, Sam Lindley,
Julian Lange, Hans Hüttel, and Luís Caires for fruitful discussions
and pointers.

% We recommend abbrvnat bibliography style.

\bibliographystyle{abbrvnat}
\bibliography{biblio}

% The bibliography should be embedded for final submission.

% \begin{thebibliography}{}
% \softraggedright

% \bibitem[Smith et~al.(2009)Smith, Jones]{smith02}
% P. Q. Smith, and X. Y. Jones. ...reference text...

% \end{thebibliography}


% \clearpage{}
% \input{appendix}


\end{document}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: t
%%% End:
                                                                                                                  doc/ICFP2016/fig-wellformed-contexts.tex                                                            0000664 0001750 0001750 00000002224 13216017506 020230  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[t]
  Context formation, $\Delta \vdash \Gamma$
  %
  \begin{gather*}
    \Delta \vdash \cdot
    \quad
    \frac{
      \Delta \vdash \Gamma
      \quad
      \Delta \vdash T :: k
    }{
      \Delta \vdash \Gamma, x\colon T
    }
  \end{gather*}

  Context splitting, $\Delta \vdash \Gamma = \Gamma \circ \Gamma$
  % 
  \begin{gather*}
    \Delta \vdash \cdot = \cdot \circ \cdot
    \qquad
    \frac{
      \Delta \vdash \Gamma = \Gamma_1 \circ \Gamma_2
      \quad
      \un_\Delta(T)
    }{
      \Delta \vdash \Gamma, x\colon T = (\Gamma_1,x\colon T) \circ (\Gamma_2,x\colon T)
    }
    \\
    \frac{
      \Delta \vdash \Gamma = \Gamma_1 \circ \Gamma_2
      \quad
      \lin_\Delta(T)
    }{
      \Delta \vdash \Gamma, x\colon T = (\Gamma_1,x\colon T) \circ \Gamma_2
    }
    \;\;%\quad
    \frac{
      \Delta \vdash \Gamma = \Gamma_1 \circ \Gamma_2
    \quad
      \lin_\Delta(T)
    }{
      \Delta \vdash \Gamma, x\colon T = \Gamma_1 \circ (\Gamma_2,x\colon T)
    }
  \end{gather*}
  
  \caption{Typing context formation and splitting}
  \label{fig:contexts}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                                                            doc/ICFP2016/sigplanconf.cls                                                                        0000664 0001750 0001750 00000113410 13216017506 015744  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               %-----------------------------------------------------------------------------
%
%               LaTeX Class/Style File
%
% Name:         sigplanconf.cls
%
% Purpose:      A LaTeX 2e class file for SIGPLAN conference proceedings.
%               This class file supercedes acm_proc_article-sp,
%               sig-alternate, and sigplan-proc.
%
% Author:       Paul C. Anagnostopoulos
%               Windfall Software
%               978 371-2316
%               paul [atsign] windfall.com
%
% Created:      12 September 2004
%
% Revisions:    See end of file.
%
% This work is licensed under the Creative Commons Attribution License.
% To view a copy of this license, visit
%   http://creativecommons.org/licenses/by/3.0/
% or send a letter to Creative Commons, 171 2nd Street, Suite 300,
% San Francisco, California, 94105, U.S.A.
%
%-----------------------------------------------------------------------------


\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{sigplanconf}[2013/07/02 v2.8 ACM SIGPLAN Proceedings]

% The following few pages contain LaTeX programming extensions adapted
% from the ZzTeX macro package.

%                       Token Hackery
%                       ----- -------


\def \@expandaftertwice {\expandafter\expandafter\expandafter}
\def \@expandafterthrice {\expandafter\expandafter\expandafter\expandafter
                          \expandafter\expandafter\expandafter}

% This macro discards the next token.

\def \@discardtok #1{}%                                  token

% This macro removes the `pt' following a dimension.

{\catcode `\p = 12 \catcode `\t = 12

\gdef \@remover #1pt{#1}

} % \catcode

% This macro extracts the contents of a macro and returns it as plain text.
% Usage: \expandafter\@defof \meaning\macro\@mark

\def \@defof #1:->#2\@mark{#2}

%                       Control Sequence Names
%                       ------- -------- -----


\def \@name #1{%                                        {\tokens}
  \csname \expandafter\@discardtok \string#1\endcsname}

\def \@withname #1#2{%                                  {\command}{\tokens}
  \expandafter#1\csname \expandafter\@discardtok \string#2\endcsname}

%                       Flags (Booleans)
%                       ----- ----------

% The boolean literals \@true and \@false are appropriate for use with
% the \if command, which tests the codes of the next two characters.

\def \@true {TT}
\def \@false {FL}

\def \@setflag #1=#2{\edef #1{#2}}%              \flag = boolean

%                       IF and Predicates
%                       -- --- ----------

% A "predicate" is a macro that returns \@true or \@false as its value.
% Such values are suitable for use with the \if conditional.  For example:
%
%   \if \@oddp{\x} <then-clause> \else <else-clause> \fi

% A predicate can be used with \@setflag as follows:
%
%   \@setflag \flag = {<predicate>}

% Here are the predicates for TeX's repertoire of conditional
% commands.  These might be more appropriately interspersed with
% other definitions in this module, but what the heck.
% Some additional "obvious" predicates are defined.

\def \@eqlp   #1#2{\ifnum #1 = #2\@true \else \@false \fi}
\def \@neqlp  #1#2{\ifnum #1 = #2\@false \else \@true \fi}
\def \@lssp   #1#2{\ifnum #1 < #2\@true \else \@false \fi}
\def \@gtrp   #1#2{\ifnum #1 > #2\@true \else \@false \fi}
\def \@zerop  #1{\ifnum #1 = 0\@true \else \@false \fi}
\def \@onep   #1{\ifnum #1 = 1\@true \else \@false \fi}
\def \@posp   #1{\ifnum #1 > 0\@true \else \@false \fi}
\def \@negp   #1{\ifnum #1 < 0\@true \else \@false \fi}
\def \@oddp   #1{\ifodd #1\@true \else \@false \fi}
\def \@evenp  #1{\ifodd #1\@false \else \@true \fi}
\def \@rangep #1#2#3{\if \@orp{\@lssp{#1}{#2}}{\@gtrp{#1}{#3}}\@false \else
                                                          \@true \fi}
\def \@tensp  #1{\@rangep{#1}{10}{19}}

\def \@dimeqlp   #1#2{\ifdim #1 = #2\@true \else \@false \fi}
\def \@dimneqlp  #1#2{\ifdim #1 = #2\@false \else \@true \fi}
\def \@dimlssp   #1#2{\ifdim #1 < #2\@true \else \@false \fi}
\def \@dimgtrp   #1#2{\ifdim #1 > #2\@true \else \@false \fi}
\def \@dimzerop  #1{\ifdim #1 = 0pt\@true \else \@false \fi}
\def \@dimposp   #1{\ifdim #1 > 0pt\@true \else \@false \fi}
\def \@dimnegp   #1{\ifdim #1 < 0pt\@true \else \@false \fi}

\def \@vmodep     {\ifvmode \@true \else \@false \fi}
\def \@hmodep     {\ifhmode \@true \else \@false \fi}
\def \@mathmodep  {\ifmmode \@true \else \@false \fi}
\def \@textmodep  {\ifmmode \@false \else \@true \fi}
\def \@innermodep {\ifinner \@true \else \@false \fi}

\long\def \@codeeqlp #1#2{\if #1#2\@true \else \@false \fi}

\long\def \@cateqlp #1#2{\ifcat #1#2\@true \else \@false \fi}

\long\def \@tokeqlp  #1#2{\ifx #1#2\@true \else \@false \fi}
\long\def \@xtokeqlp #1#2{\expandafter\ifx #1#2\@true \else \@false \fi}

\long\def \@definedp #1{%
  \expandafter\ifx \csname \expandafter\@discardtok \string#1\endcsname
                   \relax \@false \else \@true \fi}

\long\def \@undefinedp #1{%
  \expandafter\ifx \csname \expandafter\@discardtok \string#1\endcsname
                   \relax \@true \else \@false \fi}

\def \@emptydefp #1{\ifx #1\@empty \@true \else \@false \fi}%       {\name}

\let \@emptylistp = \@emptydefp

\long\def \@emptyargp #1{%                               {#n}
  \@empargp #1\@empargq\@mark}
\long\def \@empargp #1#2\@mark{%
  \ifx #1\@empargq \@true \else \@false \fi}
\def \@empargq {\@empargq}

\def \@emptytoksp #1{%                                   {\tokenreg}
  \expandafter\@emptoksp \the#1\@mark}

\long\def \@emptoksp #1\@mark{\@emptyargp{#1}}

\def \@voidboxp #1{\ifvoid #1\@true \else \@false \fi}
\def \@hboxp #1{\ifhbox #1\@true \else \@false \fi}
\def \@vboxp #1{\ifvbox #1\@true \else \@false \fi}

\def \@eofp #1{\ifeof #1\@true \else \@false \fi}


% Flags can also be used as predicates, as in:
%
%   \if \flaga <then-clause> \else <else-clause> \fi


% Now here we have predicates for the common logical operators.

\def \@notp #1{\if #1\@false \else \@true \fi}

\def \@andp #1#2{\if #1%
                  \if #2\@true \else \@false \fi
                \else
                  \@false
                \fi}

\def \@orp #1#2{\if #1%
                 \@true
               \else
                 \if #2\@true \else \@false \fi
               \fi}

\def \@xorp #1#2{\if #1%
                  \if #2\@false \else \@true \fi
                \else
                  \if #2\@true \else \@false \fi
                \fi}

%                       Arithmetic
%                       ----------

\def \@increment #1{\advance #1 by 1\relax}%             {\count}

\def \@decrement #1{\advance #1 by -1\relax}%            {\count}

%                       Options
%                       -------


\@setflag \@authoryear = \@false
\@setflag \@blockstyle = \@false
\@setflag \@copyrightwanted = \@true
\@setflag \@explicitsize = \@false
\@setflag \@mathtime = \@false
\@setflag \@natbib = \@true
\@setflag \@ninepoint = \@true
\newcount{\@numheaddepth} \@numheaddepth = 3
\@setflag \@onecolumn = \@false
\@setflag \@preprint = \@false
\@setflag \@reprint = \@false
\@setflag \@tenpoint = \@false
\@setflag \@times = \@false

% Note that all the dangerous article class options are trapped.

\DeclareOption{9pt}{\@setflag \@ninepoint = \@true
                    \@setflag \@explicitsize = \@true}

\DeclareOption{10pt}{\PassOptionsToClass{10pt}{article}%
                     \@setflag \@ninepoint = \@false
                     \@setflag \@tenpoint = \@true
                     \@setflag \@explicitsize = \@true}

\DeclareOption{11pt}{\PassOptionsToClass{11pt}{article}%
                     \@setflag \@ninepoint = \@false
                     \@setflag \@explicitsize = \@true}

\DeclareOption{12pt}{\@unsupportedoption{12pt}}

\DeclareOption{a4paper}{\@unsupportedoption{a4paper}}

\DeclareOption{a5paper}{\@unsupportedoption{a5paper}}

\DeclareOption{authoryear}{\@setflag \@authoryear = \@true}

\DeclareOption{b5paper}{\@unsupportedoption{b5paper}}

\DeclareOption{blockstyle}{\@setflag \@blockstyle = \@true}

\DeclareOption{cm}{\@setflag \@times = \@false}

\DeclareOption{computermodern}{\@setflag \@times = \@false}

\DeclareOption{executivepaper}{\@unsupportedoption{executivepaper}}

\DeclareOption{indentedstyle}{\@setflag \@blockstyle = \@false}

\DeclareOption{landscape}{\@unsupportedoption{landscape}}

\DeclareOption{legalpaper}{\@unsupportedoption{legalpaper}}

\DeclareOption{letterpaper}{\@unsupportedoption{letterpaper}}

\DeclareOption{mathtime}{\@setflag \@mathtime = \@true}

\DeclareOption{natbib}{\@setflag \@natbib = \@true}

\DeclareOption{nonatbib}{\@setflag \@natbib = \@false}

\DeclareOption{nocopyrightspace}{\@setflag \@copyrightwanted = \@false}

\DeclareOption{notitlepage}{\@unsupportedoption{notitlepage}}

\DeclareOption{numberedpars}{\@numheaddepth = 4}

\DeclareOption{numbers}{\@setflag \@authoryear = \@false}

%%%\DeclareOption{onecolumn}{\@setflag \@onecolumn = \@true}

\DeclareOption{preprint}{\@setflag \@preprint = \@true}

\DeclareOption{reprint}{\@setflag \@reprint = \@true}

\DeclareOption{times}{\@setflag \@times = \@true}

\DeclareOption{titlepage}{\@unsupportedoption{titlepage}}

\DeclareOption{twocolumn}{\@setflag \@onecolumn = \@false}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

\ExecuteOptions{9pt,indentedstyle,times}
\@setflag \@explicitsize = \@false
\ProcessOptions

\if \@onecolumn
  \if \@notp{\@explicitsize}%
    \@setflag \@ninepoint = \@false
    \PassOptionsToClass{11pt}{article}%
  \fi
  \PassOptionsToClass{twoside,onecolumn}{article}
\else
  \PassOptionsToClass{twoside,twocolumn}{article}
\fi
\LoadClass{article}

\def \@unsupportedoption #1{%
  \ClassError{proc}{The standard '#1' option is not supported.}}

% This can be used with the 'reprint' option to get the final folios.

\def \setpagenumber #1{%
  \setcounter{page}{#1}}

\AtEndDocument{\label{sigplanconf@finalpage}}

%                       Utilities
%                       ---------


\newcommand{\setvspace}[2]{%
  #1 = #2
  \advance #1 by -1\parskip}

%                       Document Parameters
%                       -------- ----------


% Page:

\setlength{\hoffset}{-1in}
\setlength{\voffset}{-1in}

\setlength{\topmargin}{1in}
\setlength{\headheight}{0pt}
\setlength{\headsep}{0pt}

\if \@onecolumn
  \setlength{\evensidemargin}{.75in}
  \setlength{\oddsidemargin}{.75in}
\else
  \setlength{\evensidemargin}{.75in}
  \setlength{\oddsidemargin}{.75in}
\fi

% Text area:

\newdimen{\standardtextwidth}
\setlength{\standardtextwidth}{42pc}

\if \@onecolumn
  \setlength{\textwidth}{40.5pc}
\else
  \setlength{\textwidth}{\standardtextwidth}
\fi

\setlength{\topskip}{8pt}
\setlength{\columnsep}{2pc}
\setlength{\textheight}{54.5pc}

% Running foot:

\setlength{\footskip}{30pt}

% Paragraphs:

\if \@blockstyle
  \setlength{\parskip}{5pt plus .1pt minus .5pt}
  \setlength{\parindent}{0pt}
\else
  \setlength{\parskip}{0pt}
  \setlength{\parindent}{12pt}
\fi

\setlength{\lineskip}{.5pt}
\setlength{\lineskiplimit}{\lineskip}

\frenchspacing
\pretolerance = 400
\tolerance = \pretolerance
\setlength{\emergencystretch}{5pt}
\clubpenalty = 10000
\widowpenalty = 10000
\setlength{\hfuzz}{.5pt}

% Standard vertical spaces:

\newskip{\standardvspace}
\setvspace{\standardvspace}{5pt plus 1pt minus .5pt}

% Margin paragraphs:

\setlength{\marginparwidth}{36pt}
\setlength{\marginparsep}{2pt}
\setlength{\marginparpush}{8pt}


\setlength{\skip\footins}{8pt plus 3pt minus 1pt}
\setlength{\footnotesep}{9pt}

\renewcommand{\footnoterule}{%
  \hrule width .5\columnwidth height .33pt depth 0pt}

\renewcommand{\@makefntext}[1]{%
  \noindent \@makefnmark \hspace{1pt}#1}

% Floats:

\setcounter{topnumber}{4}
\setcounter{bottomnumber}{1}
\setcounter{totalnumber}{4}

\renewcommand{\fps@figure}{tp}
\renewcommand{\fps@table}{tp}
\renewcommand{\topfraction}{0.90}
\renewcommand{\bottomfraction}{0.30}
\renewcommand{\textfraction}{0.10}
\renewcommand{\floatpagefraction}{0.75}

\setcounter{dbltopnumber}{4}

\renewcommand{\dbltopfraction}{\topfraction}
\renewcommand{\dblfloatpagefraction}{\floatpagefraction}

\setlength{\floatsep}{18pt plus 4pt minus 2pt}
\setlength{\textfloatsep}{18pt plus 4pt minus 3pt}
\setlength{\intextsep}{10pt plus 4pt minus 3pt}

\setlength{\dblfloatsep}{18pt plus 4pt minus 2pt}
\setlength{\dbltextfloatsep}{20pt plus 4pt minus 3pt}

% Miscellaneous:

\errorcontextlines = 5

%                       Fonts
%                       -----


\if \@times
  \renewcommand{\rmdefault}{ptm}%
  \if \@mathtime
    \usepackage[mtbold,noTS1]{mathtime}%
  \else
%%%    \usepackage{mathptm}%
  \fi
\else
  \relax
\fi

\if \@ninepoint

\renewcommand{\normalsize}{%
  \@setfontsize{\normalsize}{9pt}{10pt}%
  \setlength{\abovedisplayskip}{5pt plus 1pt minus .5pt}%
  \setlength{\belowdisplayskip}{\abovedisplayskip}%
  \setlength{\abovedisplayshortskip}{3pt plus 1pt minus 2pt}%
  \setlength{\belowdisplayshortskip}{\abovedisplayshortskip}}

\renewcommand{\tiny}{\@setfontsize{\tiny}{5pt}{6pt}}

\renewcommand{\scriptsize}{\@setfontsize{\scriptsize}{7pt}{8pt}}

\renewcommand{\small}{%
  \@setfontsize{\small}{8pt}{9pt}%
  \setlength{\abovedisplayskip}{4pt plus 1pt minus 1pt}%
  \setlength{\belowdisplayskip}{\abovedisplayskip}%
  \setlength{\abovedisplayshortskip}{2pt plus 1pt}%
  \setlength{\belowdisplayshortskip}{\abovedisplayshortskip}}

\renewcommand{\footnotesize}{%
  \@setfontsize{\footnotesize}{8pt}{9pt}%
  \setlength{\abovedisplayskip}{4pt plus 1pt minus .5pt}%
  \setlength{\belowdisplayskip}{\abovedisplayskip}%
  \setlength{\abovedisplayshortskip}{2pt plus 1pt}%
  \setlength{\belowdisplayshortskip}{\abovedisplayshortskip}}

\renewcommand{\large}{\@setfontsize{\large}{11pt}{13pt}}

\renewcommand{\Large}{\@setfontsize{\Large}{14pt}{18pt}}

\renewcommand{\LARGE}{\@setfontsize{\LARGE}{18pt}{20pt}}

\renewcommand{\huge}{\@setfontsize{\huge}{20pt}{25pt}}

\renewcommand{\Huge}{\@setfontsize{\Huge}{25pt}{30pt}}

\else\if \@tenpoint

\relax

\else

\relax

\fi\fi

%                       Abstract
%                       --------


\renewenvironment{abstract}{%
  \section*{Abstract}%
  \normalsize}{%
  }

%                       Bibliography
%                       ------------


\renewenvironment{thebibliography}[1]
     {\section*{\refname
        \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \bibfont
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

\if \@natbib

\if \@authoryear
  \typeout{Using natbib package with 'authoryear' citation style.}
  \usepackage[authoryear,square]{natbib}
  \bibpunct{(}{)}{;}{a}{}{,}    % Change fences to parentheses;
                                % citation separator to semicolon;
                                % eliminate comma between author and year.
  \let \cite = \citep
\else
  \typeout{Using natbib package with 'numbers' citation style.}
  \usepackage[numbers,sort&compress,square]{natbib}
\fi
\setlength{\bibsep}{3pt plus .5pt minus .25pt}

\fi

\def \bibfont {\small}

%                       Categories
%                       ----------


\@setflag \@firstcategory = \@true

\newcommand{\category}[3]{%
  \if \@firstcategory
    \paragraph*{Categories and Subject Descriptors}%
    \@setflag \@firstcategory = \@false
  \else
    \unskip ;\hspace{.75em}%
  \fi
  \@ifnextchar [{\@category{#1}{#2}{#3}}{\@category{#1}{#2}{#3}[]}}

\def \@category #1#2#3[#4]{%
  {\let \and = \relax
   #1 [\textit{#2}]%
   \if \@emptyargp{#4}%
     \if \@notp{\@emptyargp{#3}}: #3\fi
   \else
     :\space
     \if \@notp{\@emptyargp{#3}}#3---\fi
     \textrm{#4}%
   \fi}}

%                       Copyright Notice
%                       --------- ------


\def \ftype@copyrightbox {8}
\def \@toappear {}
\def \@permission {}
\def \@reprintprice {}

\def \@copyrightspace {%
  \@float{copyrightbox}[b]%
  \vbox to 1.2in{%
    \vfill
    \parbox[b]{20pc}{%
      \scriptsize
      \if \@preprint
        [Copyright notice will appear here
         once 'preprint' option is removed.]\par
      \else
        \@toappear
      \fi
      \if \@reprint
        \noindent Reprinted from \@conferencename,
        \@proceedings,
        \@conferenceinfo,
        pp.~\number\thepage--\pageref{sigplanconf@finalpage}.\par
      \fi}}%
  \end@float}

\newcommand{\reprintprice}[1]{%
  \gdef \@reprintprice {#1}}

\reprintprice{\$15.00}

\long\def \toappear #1{%
  \def \@toappear {#1}}

\toappear{%
  \noindent \@permission \par
  \vspace{2pt}
  \noindent \textsl{\@conferencename}, \quad \@conferenceinfo. \par
  \noindent Copyright \copyright\ \@copyrightyear\ ACM \@copyrightdata
    \dots \@reprintprice.\par
  \noindent http://dx.doi.org/10.1145/\@doi }

\newcommand{\permission}[1]{%
  \gdef \@permission {#1}}

\permission{%
  Permission to make digital or hard copies of all or part of this work for
  personal or classroom use is granted without fee provided that copies are
  not made or distributed for profit or commercial advantage and that copies
  bear this notice and the full citation on the first page. Copyrights for
  components of this work owned by others than ACM must be honored.
  Abstracting with credit is permitted. To copy otherwise, or republish, to
  post on servers or to redistribute to lists, requires prior specific
  permission and/or a fee. Request permissions from permissions@acm.org.}

% These are two new rights management and bibstrip text blocks.

\newcommand{\exclusivelicense}{%
  \permission{%
    Permission to make digital or hard copies of all or part of this work for
    personal or classroom use is granted without fee provided that copies are
    not made or distributed for profit or commercial advantage and that copies
    bear this notice and the full citation on the first page. Copyrights for
    components of this work owned by others than the author(s) must be honored.
    Abstracting with credit is permitted. To copy otherwise, or republish, to
    post on servers or to redistribute to lists, requires prior specific
    permission and/or a fee. Request permissions from permissions@acm.org.}
  \toappear{%
    \noindent \@permission \par
    \vspace{2pt}
    \noindent \textsl{\@conferencename}, \quad \@conferenceinfo. \par
    \noindent Copyright is held by the owner/author(s). Publication rights licensed to ACM. \par
    \noindent ACM \@copyrightdata \dots \@reprintprice.\par
    \noindent http://dx.doi.org/10.1145/\@doi}}

\newcommand{\permissiontopublish}{%
  \permission{%
    Permission to make digital or hard copies of part or all of this work for
    personal or classroom use is granted without fee provided that copies are
    not made or distributed for profit or commercial advantage and that copies
    bear this notice and the full citation on the first page. Copyrights for
    third-party components of this work must be honored. 
    For all other uses, contact the owner/author(s).}%
  \toappear{%
    \noindent \@permission \par
    \vspace{2pt}
    \noindent \textsl{\@conferencename}, \quad \@conferenceinfo. \par
    \noindent Copyright is held by the owner/author(s). \par
    \noindent ACM \@copyrightdata.\par
    \noindent http://dx.doi.org/10.1145/\@doi}}

% The following permission notices are
% for the traditional copyright transfer agreement option.

% Exclusive license and permission-to-publish 
% give more complicated permission notices.
% These are not covered here.

\newcommand{\ACMCanadapermission}{%
  \permission{%
    ACM acknowledges that this contribution was authored or
    co-authored by an affiliate of the Canadian National
    Government. As such, the Crown in Right of Canada retains an equal
    interest in the copyright. Reprint requests should be forwarded to
    ACM.}}

\newcommand{\ACMUSpermission}{%
  \permission{%
    ACM acknowledges that this contribution was authored or
    co-authored by a contractor or affiliate of the United States
    Government. As such, the United States Government retains a
    nonexclusive, royalty-free right to publish or reproduce this
    article, or to allow others to do so, for Government purposes
    only.}}

\newcommand{\USpublicpermission}{%
  \permission{%
    This paper is authored by an employee(s) of the United States
    Government and is in the public domain. Non-exclusive copying or
    redistribution is allowed, provided that the article citation is
    given and the authors and the agency are clearly identified as its
    source.}%
  \toappear{%
    \noindent \@permission \par
    \vspace{2pt}
    \noindent \textsl{\@conferencename}, \quad \@conferenceinfo. \par
    \noindent ACM \@copyrightdata.\par
    \noindent http://dx.doi.org/10.1145/\@doi}}

\newcommand{\authorversion}[4]{%
  \permission{%
  Copyright \copyright\ ACM, #1. This is the author's version of the work.
  It is posted here by permission of ACM for your personal use.
  Not for redistribution. The definitive version was published in
  #2, #3, http://dx.doi.org/10.1145/#4.}}

%                       Enunciations
%                       ------------


\def \@begintheorem #1#2{%                      {name}{number}
  \trivlist
  \item[\hskip \labelsep \textsc{#1 #2.}]%
  \itshape\selectfont
  \ignorespaces}

\def \@opargbegintheorem #1#2#3{%               {name}{number}{title}
  \trivlist
  \item[%
    \hskip\labelsep \textsc{#1\ #2}%
    \if \@notp{\@emptyargp{#3}}\nut (#3).\fi]%
  \itshape\selectfont
  \ignorespaces}

%                       Figures
%                       -------


\@setflag \@caprule = \@true

\long\def \@makecaption #1#2{%
  \addvspace{4pt}
  \if \@caprule
    \hrule width \hsize height .33pt
    \vspace{4pt}
  \fi
  \setbox \@tempboxa = \hbox{\@setfigurenumber{#1.}\nut #2}%
  \if \@dimgtrp{\wd\@tempboxa}{\hsize}%
    \noindent \@setfigurenumber{#1.}\nut #2\par
  \else
    \centerline{\box\@tempboxa}%
  \fi}

\newcommand{\nocaptionrule}{%
  \@setflag \@caprule = \@false}

\def \@setfigurenumber #1{%
  {\rmfamily \bfseries \selectfont #1}}

%                       Hierarchy
%                       ---------


\setcounter{secnumdepth}{\@numheaddepth}

\newskip{\@sectionaboveskip}
\setvspace{\@sectionaboveskip}{10pt plus 3pt minus 2pt}

\newskip{\@sectionbelowskip}
\if \@blockstyle
  \setlength{\@sectionbelowskip}{0.1pt}%
\else
  \setlength{\@sectionbelowskip}{4pt}%
\fi

\renewcommand{\section}{%
  \@startsection
    {section}%
    {1}%
    {0pt}%
    {-\@sectionaboveskip}%
    {\@sectionbelowskip}%
    {\large \bfseries \raggedright}}

\newskip{\@subsectionaboveskip}
\setvspace{\@subsectionaboveskip}{8pt plus 2pt minus 2pt}

\newskip{\@subsectionbelowskip}
\if \@blockstyle
  \setlength{\@subsectionbelowskip}{0.1pt}%
\else
  \setlength{\@subsectionbelowskip}{4pt}%
\fi

\renewcommand{\subsection}{%
  \@startsection%
    {subsection}%
    {2}%
    {0pt}%
    {-\@subsectionaboveskip}%
    {\@subsectionbelowskip}%
    {\normalsize \bfseries \raggedright}}

\renewcommand{\subsubsection}{%
  \@startsection%
    {subsubsection}%
    {3}%
    {0pt}%
    {-\@subsectionaboveskip}
    {\@subsectionbelowskip}%
    {\normalsize \bfseries \raggedright}}

\newskip{\@paragraphaboveskip}
\setvspace{\@paragraphaboveskip}{6pt plus 2pt minus 2pt}

\renewcommand{\paragraph}{%
  \@startsection%
    {paragraph}%
    {4}%
    {0pt}%
    {\@paragraphaboveskip}
    {-1em}%
    {\normalsize \bfseries \if \@times \itshape \fi}}

\renewcommand{\subparagraph}{%
  \@startsection%
    {subparagraph}%
    {4}%
    {0pt}%
    {\@paragraphaboveskip}
    {-1em}%
    {\normalsize \itshape}}

% Standard headings:

\newcommand{\acks}{\section*{Acknowledgments}}

\newcommand{\keywords}{\paragraph*{Keywords}}

\newcommand{\terms}{\paragraph*{General Terms}}

%                       Identification
%                       --------------


\def \@conferencename {}
\def \@conferenceinfo {}
\def \@copyrightyear {}
\def \@copyrightdata {[to be supplied]}
\def \@proceedings {[Unknown Proceedings]}


\newcommand{\conferenceinfo}[2]{%
  \gdef \@conferencename {#1}%
  \gdef \@conferenceinfo {#2}}

\newcommand{\copyrightyear}[1]{%
  \gdef \@copyrightyear {#1}}

\let \CopyrightYear = \copyrightyear

\newcommand{\copyrightdata}[1]{%
  \gdef \@copyrightdata {#1}}

\let \crdata = \copyrightdata

\newcommand{\doi}[1]{%
  \gdef \@doi {#1}}

\newcommand{\proceedings}[1]{%
  \gdef \@proceedings {#1}}

%                       Lists
%                       -----


\setlength{\leftmargini}{13pt}
\setlength\leftmarginii{13pt}
\setlength\leftmarginiii{13pt}
\setlength\leftmarginiv{13pt}
\setlength{\labelsep}{3.5pt}

\setlength{\topsep}{\standardvspace}
\if \@blockstyle
  \setlength{\itemsep}{1pt}
  \setlength{\parsep}{3pt}
\else
  \setlength{\itemsep}{1pt}
  \setlength{\parsep}{3pt}
\fi

\renewcommand{\labelitemi}{{\small \centeroncapheight{\textbullet}}}
\renewcommand{\labelitemii}{\centeroncapheight{\rule{2.5pt}{2.5pt}}}
\renewcommand{\labelitemiii}{$-$}
\renewcommand{\labelitemiv}{{\Large \textperiodcentered}}

\renewcommand{\@listi}{%
  \leftmargin = \leftmargini
  \listparindent = 0pt}
%%%  \itemsep = 1pt
%%%  \parsep = 3pt}
%%%  \listparindent = \parindent}

\let \@listI = \@listi

\renewcommand{\@listii}{%
  \leftmargin = \leftmarginii
  \topsep = 1pt
  \labelwidth = \leftmarginii
  \advance \labelwidth by -\labelsep
  \listparindent = \parindent}

\renewcommand{\@listiii}{%
  \leftmargin = \leftmarginiii
  \labelwidth = \leftmarginiii
  \advance \labelwidth by -\labelsep
  \listparindent = \parindent}

\renewcommand{\@listiv}{%
  \leftmargin = \leftmarginiv
  \labelwidth = \leftmarginiv
  \advance \labelwidth by -\labelsep
  \listparindent = \parindent}

%                       Mathematics
%                       -----------


\def \theequation {\arabic{equation}}

%                       Miscellaneous
%                       -------------


\newcommand{\balancecolumns}{%
  \vfill\eject
  \global\@colht = \textheight
  \global\ht\@cclv = \textheight}

\newcommand{\nut}{\hspace{.5em}}

\newcommand{\softraggedright}{%
  \let \\ = \@centercr
  \leftskip = 0pt
  \rightskip = 0pt plus 10pt}

%                       Program Code
%                       ------- ----


\newcommand{\mono}[1]{%
  {\@tempdima = \fontdimen2\font
   \texttt{\spaceskip = 1.1\@tempdima #1}}}

%                       Running Heads and Feet
%                       ------- ----- --- ----


\def \@preprintfooter {}

\newcommand{\preprintfooter}[1]{%
  \gdef \@preprintfooter {#1}}

\if \@preprint

\def \ps@plain {%
  \let \@mkboth = \@gobbletwo
  \let \@evenhead = \@empty
  \def \@evenfoot {\scriptsize
                   \rlap{\textit{\@preprintfooter}}\hfil
                   \thepage \hfil
                   \llap{\textit{\@formatyear}}}%
  \let \@oddhead = \@empty
  \let \@oddfoot = \@evenfoot}

\else\if \@reprint

\def \ps@plain {%
  \let \@mkboth = \@gobbletwo
  \let \@evenhead = \@empty
  \def \@evenfoot {\scriptsize \hfil \thepage \hfil}%
  \let \@oddhead = \@empty
  \let \@oddfoot = \@evenfoot}

\else

\let \ps@plain = \ps@empty
\let \ps@headings = \ps@empty
\let \ps@myheadings = \ps@empty

\fi\fi

\def \@formatyear {%
  \number\year/\number\month/\number\day}

%                       Special Characters
%                       ------- ----------


\DeclareRobustCommand{\euro}{%
  \protect{\rlap{=}}{\sf \kern .1em C}}

%                       Title Page
%                       ----- ----


\@setflag \@addauthorsdone = \@false

\def \@titletext {\@latex@error{No title was provided}{}}
\def \@subtitletext {}

\newcount{\@authorcount}

\newcount{\@titlenotecount}
\newtoks{\@titlenotetext}

\def \@titlebanner {}

\renewcommand{\title}[1]{%
  \gdef \@titletext {#1}}

\newcommand{\subtitle}[1]{%
  \gdef \@subtitletext {#1}}

\newcommand{\authorinfo}[3]{%           {names}{affiliation}{email/URL}
  \global\@increment \@authorcount
  \@withname\gdef {\@authorname\romannumeral\@authorcount}{#1}%
  \@withname\gdef {\@authoraffil\romannumeral\@authorcount}{#2}%
  \@withname\gdef {\@authoremail\romannumeral\@authorcount}{#3}}

\renewcommand{\author}[1]{%
  \@latex@error{The \string\author\space command is obsolete;
                use \string\authorinfo}{}}

\newcommand{\titlebanner}[1]{%
  \gdef \@titlebanner {#1}}

\renewcommand{\maketitle}{%
  \pagestyle{plain}%
  \if \@onecolumn
    {\hsize = \standardtextwidth
     \@maketitle}%
  \else
    \twocolumn[\@maketitle]%
  \fi
  \@placetitlenotes
  \if \@copyrightwanted \@copyrightspace \fi}

\def \@maketitle {%
  \begin{center}
  \@settitlebanner
  \let \thanks = \titlenote
  {\leftskip = 0pt plus 0.25\linewidth
   \rightskip = 0pt plus 0.25 \linewidth
   \parfillskip = 0pt
   \spaceskip = .7em
   \noindent \LARGE \bfseries \@titletext \par}
  \vskip 6pt
  \noindent \Large \@subtitletext \par
  \vskip 12pt
  \ifcase \@authorcount
    \@latex@error{No authors were specified for this paper}{}\or
    \@titleauthors{i}{}{}\or
    \@titleauthors{i}{ii}{}\or
    \@titleauthors{i}{ii}{iii}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\@titleauthors{x}{}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\@titleauthors{x}{xi}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\@titleauthors{x}{xi}{xii}%
  \else
    \@latex@error{Cannot handle more than 12 authors}{}%
  \fi
  \vspace{1.75pc}
  \end{center}}

\def \@settitlebanner {%
  \if \@andp{\@preprint}{\@notp{\@emptydefp{\@titlebanner}}}%
    \vbox to 0pt{%
      \vskip -32pt
      \noindent \textbf{\@titlebanner}\par
      \vss}%
    \nointerlineskip
  \fi}

\def \@titleauthors #1#2#3{%
  \if \@andp{\@emptyargp{#2}}{\@emptyargp{#3}}%
    \noindent \@setauthor{40pc}{#1}{\@false}\par
  \else\if \@emptyargp{#3}%
    \noindent \@setauthor{17pc}{#1}{\@false}\hspace{3pc}%
              \@setauthor{17pc}{#2}{\@false}\par
  \else
    \noindent \@setauthor{12.5pc}{#1}{\@false}\hspace{2pc}%
              \@setauthor{12.5pc}{#2}{\@false}\hspace{2pc}%
              \@setauthor{12.5pc}{#3}{\@true}\par
    \relax
  \fi\fi
  \vspace{20pt}}

\def \@setauthor #1#2#3{%                       {width}{text}{unused}
  \vtop{%
    \def \and {%
      \hspace{16pt}}
    \hsize = #1
    \normalfont
    \centering
    \large \@name{\@authorname#2}\par
    \vspace{5pt}
    \normalsize \@name{\@authoraffil#2}\par
    \vspace{2pt}
    \textsf{\@name{\@authoremail#2}}\par}}

\def \@maybetitlenote #1{%
  \if \@andp{#1}{\@gtrp{\@authorcount}{3}}%
    \titlenote{See page~\pageref{@addauthors} for additional authors.}%
  \fi}

\newtoks{\@fnmark}

\newcommand{\titlenote}[1]{%
  \global\@increment \@titlenotecount
  \ifcase \@titlenotecount \relax \or
    \@fnmark = {\ast}\or
    \@fnmark = {\dagger}\or
    \@fnmark = {\ddagger}\or
    \@fnmark = {\S}\or
    \@fnmark = {\P}\or
    \@fnmark = {\ast\ast}%
  \fi
  \,$^{\the\@fnmark}$%
  \edef \reserved@a {\noexpand\@appendtotext{%
                       \noexpand\@titlefootnote{\the\@fnmark}}}%
  \reserved@a{#1}}

\def \@appendtotext #1#2{%
  \global\@titlenotetext = \expandafter{\the\@titlenotetext #1{#2}}}

\newcount{\@authori}

\iffalse
\def \additionalauthors {%
  \if \@gtrp{\@authorcount}{3}%
    \section{Additional Authors}%
    \label{@addauthors}%
    \noindent
    \@authori = 4
    {\let \\ = ,%
     \loop 
       \textbf{\@name{\@authorname\romannumeral\@authori}},
       \@name{\@authoraffil\romannumeral\@authori},
       email: \@name{\@authoremail\romannumeral\@authori}.%
       \@increment \@authori
     \if \@notp{\@gtrp{\@authori}{\@authorcount}} \repeat}%
    \par
  \fi
  \global\@setflag \@addauthorsdone = \@true}
\fi

\let \addauthorsection = \additionalauthors

\def \@placetitlenotes {
  \the\@titlenotetext}

%                       Utilities
%                       ---------


\newcommand{\centeroncapheight}[1]{%
  {\setbox\@tempboxa = \hbox{#1}%
   \@measurecapheight{\@tempdima}%         % Calculate ht(CAP) - ht(text)
   \advance \@tempdima by -\ht\@tempboxa   %           ------------------
   \divide \@tempdima by 2                 %                   2
   \raise \@tempdima \box\@tempboxa}}

\newbox{\@measbox}

\def \@measurecapheight #1{%                            {\dimen}
  \setbox\@measbox = \hbox{ABCDEFGHIJKLMNOPQRSTUVWXYZ}%
  #1 = \ht\@measbox}

\long\def \@titlefootnote #1#2{%
  \insert\footins{%
    \reset@font\footnotesize
    \interlinepenalty\interfootnotelinepenalty
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth \@parboxrestore
%%%    \protected@edef\@currentlabel{%
%%%       \csname p@footnote\endcsname\@thefnmark}%
    \color@begingroup
      \def \@makefnmark {$^{#1}$}%
      \@makefntext{%
        \rule\z@\footnotesep\ignorespaces#2\@finalstrut\strutbox}%
    \color@endgroup}}

%                       LaTeX Modifications
%                       ----- -------------

\def \@seccntformat #1{%
  \@name{\the#1}%
  \@expandaftertwice\@seccntformata \csname the#1\endcsname.\@mark
  \quad}

\def \@seccntformata #1.#2\@mark{%
  \if \@emptyargp{#2}.\fi}

%                       Revision History
%                       -------- -------


%  Date         Person  Ver.    Change
%  ----         ------  ----    ------

%  2004.09.12   PCA     0.1--4  Preliminary development.

%  2004.11.18   PCA     0.5     Start beta testing.

%  2004.11.19   PCA     0.6     Obsolete \author and replace with
%                               \authorinfo.
%                               Add 'nocopyrightspace' option.
%                               Compress article opener spacing.
%                               Add 'mathtime' option.
%                               Increase text height by 6 points.

%  2004.11.28   PCA     0.7     Add 'cm/computermodern' options.
%                               Change default to Times text.

%  2004.12.14   PCA     0.8     Remove use of mathptm.sty; it cannot
%                               coexist with latexsym or amssymb.

%  2005.01.20   PCA     0.9     Rename class file to sigplanconf.cls.

%  2005.03.05   PCA     0.91    Change default copyright data.

%  2005.03.06   PCA     0.92    Add at-signs to some macro names.

%  2005.03.07   PCA     0.93    The 'onecolumn' option defaults to '11pt',
%                               and it uses the full type width.

%  2005.03.15   PCA     0.94    Add at-signs to more macro names.
%                               Allow margin paragraphs during review.

%  2005.03.22   PCA     0.95    Implement \euro.
%                               Remove proof and newdef environments.

%  2005.05.06   PCA     1.0     Eliminate 'onecolumn' option.
%                               Change footer to small italic and eliminate
%                               left portion if no \preprintfooter.
%                               Eliminate copyright notice if preprint.
%                               Clean up and shrink copyright box.

%  2005.05.30   PCA     1.1     Add alternate permission statements.

%  2005.06.29   PCA     1.1     Publish final first edition of guide.

%  2005.07.14   PCA     1.2     Add \subparagraph.
%                               Use block paragraphs in lists, and adjust
%                               spacing between items and paragraphs.

%  2006.06.22   PCA     1.3     Add 'reprint' option and associated
%                               commands.

%  2006.08.24   PCA     1.4     Fix bug in \maketitle case command.

%  2007.03.13   PCA     1.5     The title banner only displays with the
%                               'preprint' option.

%  2007.06.06   PCA     1.6     Use \bibfont in \thebibliography.
%                               Add 'natbib' option to load and configure
%                                 the natbib package.

%  2007.11.20   PCA     1.7     Balance line lengths in centered article
%                                 title (thanks to Norman Ramsey).

%  2009.01.26   PCA     1.8     Change natbib \bibpunct values.

%  2009.03.24   PCA     1.9     Change natbib to use the 'numbers' option.
%                               Change templates to use 'natbib' option.

%  2009.09.01   PCA     2.0     Add \reprintprice command (suggested by
%                                 Stephen Chong).

%  2009.09.08   PCA     2.1     Make 'natbib' the default; add 'nonatbib'.
%               SB              Add 'authoryear' and 'numbers' (default) to
%                               control citation style when using natbib.
%                               Add \bibpunct to change punctuation for
%                               'authoryear' style.

%  2009.09.21   PCA     2.2     Add \softraggedright to the thebibliography
%                               environment. Also add to template so it will
%                               happen with natbib.

%  2009.09.30   PCA     2.3     Remove \softraggedright from thebibliography.  
%                               Just include in the template.

%  2010.05.24   PCA     2.4     Obfuscate class author's email address.

%  2011.11.08   PCA     2.5     Add copyright notice to this file.
%                               Remove 'sort' option from natbib when using
%                                 'authoryear' style.
%                               Add the \authorversion command.

%  2013.02.22   PCA     2.6     Change natbib fences to parentheses when
%                                 using 'authoryear' style.

%  2013.05.17   PCA     2.7     Change standard and author copyright text.

%  2013.07.02   TU      2.8     More changes to permission/copyright notes.
%                               Replaced ambiguous \authorpermission with
%                               \exclusivelicense and \permissiontopublish
 

                                                                                                                                                                                                                                                        doc/ICFP2016/fig-processes.tex                                                                      0000664 0001750 0001750 00000001475 13216017506 016240  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \begin{figure}[t]
  \begin{align*}
    v \grmeq& \sendk %&& \text{Values}\\
    \grmor \recvk
    \grmor ()
%    \grmor& \text{(other literals)}\\
    \grmor \lambda a.e
    \grmor (v,v)
    \grmor \inject lv
    \\
    e \grmeq& v %&& \text{Expressions}\\
    \grmor a
    \grmor \newk
    \grmor ee
    \grmor \fix ae
    \grmor (e,e)
    \grmor \ink\,l\,e\\
    \grmor& \letin{a,b} e e
    \grmor \forkk\, e
    \grmor \match e {l_i\rightarrow e_i}_{i\in I}\\
%    \grmor& \letin aee\\
    \grmor& \select le
    \grmor \casek\,e\,\ofk\,\{l_i\rightarrow e_i\}_{i\in I}
    \\
    p \grmeq& e %&& \text{Processes}\\
    \grmor p \PAR p
    \grmor (\nu a,b)p
  \end{align*}
  \caption{Values, expressions, and processes}
  \label{fig:processes}
\end{figure}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                   doc/ICFP2016/definitions.tex                                                                        0000664 0001750 0001750 00000030277 13216017506 016004  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \section{Expressions, statics, and dynamics}
\label{sec:definitions}

% PROCESSES 
\subsection{Processes}
\label{sec:processes}

\input{fig-processes}

Values, expressions, and processes in Figure~\ref{fig:processes}.

% REDUCTION
\input{fig-reduction}

Structural congruence, $\equiv$, is the smallest relation that
includes the commutative monoid rules (for $\mid$ and $()$) and scope
extrusion. Reduction in Figure~\ref{fig:reduction}.

% RUNTIME ERRORS

Runtime errors. The \emph{subject} of an expression~$e$, $\subj(e)$,
is~$x$ in the following cases and undefined in all other cases.
%
\begin{equation*}
  \send \_x \qquad
  \recv x \qquad
  \select \_x \qquad
  \case x\_
\end{equation*}

Two expressions \emph{agree} in the following cases.
%
\begin{align*}
  \agree^{xy}(\send \_x, \recv y) &\qquad
  \agree^{xy}(\recv x,\send \_y)\\
  \agree^{xy}(\select \_x, \case y\_) &\qquad
  \agree^{xy}(\case x\_,\select \_y)
\end{align*}

A process is an \emph{error} if it is structurally congruent to some
process that contains a subexpression or subprocess of one of the
following forms:
%
\begin{enumerate}
\item $\letin{xy}{v}{e}$ and $v$ is not a pair;
\item $\match{v}{l_i\rightarrow e_i}_{i\in I}$ and $v\ne(\ink\,l_j\,v')$ for some $v'$ and
  $j\in I$;
\item $E_1[e_1] \mid E_2[e_2]$ and $\subj(e_1) = \subj(e_2) = x$ where
  neither $E_1$ nor $E_2$ bind~$x$;
\item $(\new xy)(E_1[e_1] \mid E_2[e_2] \mid p)$ and $\subj(e_1)=x$
  and $\subj(e_2)=y$ and $\neg\agree^{xy}(e_1,e_2)$ where $E_1$ does not
  bind~$x$ and $E_2$ does not bind~$y$;
\item $  (\new xy)(E_1[\selectk\,l_j\,x] \mid
  E_2[\casek\,y\,\ofk\,\{l_i\rightarrow e_i\}_{i\in I}] \mid p) $
  where $E_1$ does not bind $x$ and $E_2$ does not bind $y$ and
  $j\notin I$ (or the same with $x$ and $y$ swapped).
\end{enumerate}

% TYPES

% BEGIN - Moved to the section on types
% \subsection{Types}
% \label{sec:types}


% Base sets: \emph{recursion variables}~$x$; \emph{type
%   variables}~$\alpha$; \emph{primitive types}~$B$ including for
% example $\unitk$ and $\intk$. Grammars for session types, types, and
% type schemes in Figure~\ref{fig:types}.

% % WELL FORMED TYPE EXPRESSIONS
% Not all types in the language generated by the grammar are of
% interest; as customary, we consider recursive types like $\mu x. \mu y.x$ or
% $\mu x.(x;x)$ meaningless as they only have trivial interpretations. We
% follow MacQueen at al.~\cite{MacQueen198695} and require recursive
% types to be contractive.
% %
% %\input{fig-contractivity}
% %
% A type $T$ is \emph{contractive in $x$} if $\GEnv_x \Contr T : \gamma$ is
% derivable with the rules in Figure~\ref{fig:contractivity} for
% some $\GEnv_x$ that contains \textbf{no guardedness assumption about $x$}. The metavariable
% $\gamma$ ranges over $\Guarded$ and $\Productive$. 
% The intuitive reading is that any (recursive) use of $x$ must be preceded
% by a nontrivial type construction, where nontrivial means different
% from $\skipk$. For example, a session type like $\mu x. (\skipk; x)$
% or $\mu x.(x; !B)$ must be disallowed whereas $\mu x.(!B; x)$ should
% be allowed.  If $\GEnv \Contr T : \Guarded$ is derivable, but not $\GEnv
% \Contr T : \Productive$, then $T$ is essentially composed of
% $\skipk$s. If $\GEnv \Contr T : \Productive$ is derivable, then $T$
% describes a nontrivial interaction.

% The interaction between $\mu$-operators is also nontrivial. For
% example, the type $\mu x'. \mu x. (x'; x) $ is ruled out because $x'$
% would not be guarded after unrolling $\mu x$ once. However, the type
% $\mu x'. (!B; \mu x. (x'; x))$ is accepted because unrolling $\mu x'$
% reveals that the recursive occurrence of $x$ is guarded: $(!B; \mu
% x. (\mu x'. (!B; \mu x. (x'; x)); x))$. Hence, an essential
% requirement of well-formedness of types is preservation of
% well-formedness under arbitrary unrolling of $\mu$ operators. 

% The second judgment $ \GEnv \vdash T \isOk$ in Figure~\ref{fig:contractivity}
% specifies the set of \emph{well-formed} type expressions where
% $\GEnv$ contains guardedness assumptions about the recursion
% variables that may occur in $T$. The obvious
% inductive rules that require well-formedness of every subexpression
% are omitted.

% This definition incorporates type variables $\alpha$ by assuming that they
% are always replaced by productive types.
% Type variables must be restricted in this way because contractivity of $\mu
% x. (\alpha; x)$ requires the $\alpha \Contr \alpha : \Productive$ to be derivable so that
% $\alpha \Contr \mu x. (\alpha; x) : \Productive$ is derivable. 
% %
% Finally, a type scheme $\forall \alpha_1\dots\forall\alpha_n. T$ is
% \emph{well-formed} if $\alpha_1, \dots, \alpha_n \vdash T \isOk$ is
% derivable. 

% \begin{lemma}
%   If $\GEnv \vdash T \isOk$, then $\GEnv, x : \gamma \vdash T \isOk$
%   for some $x$ not in $\GEnv$.
% \end{lemma}

% \begin{lemma}
%  If $\GEnv \vdash \mu x.T \isOk$, then $\GEnv \vdash T[\mu
%   x. T/x] \isOk$.
% \end{lemma}
% \begin{proof}
%   If  $\GEnv \vdash \mu x.T \isOk$, it must be because  $\GEnv,
%   x:\gamma \vdash T \isOk$ and $\GEnv\setminus x \Contr T : \gamma$.
%   We prove by induction on $\GEnv, x:\gamma \vdash T \isOk$ that
%   $\GEnv \vdash T[\mu  x. T/x] \isOk$.

%   There are two interesting cases. In the first case, we encounter the
%   recursion variable $\GEnv, x:\gamma, \GEnv' \vdash x
%   \isOk$. At this point, we have to return $\GEnv, \GEnv' \vdash \mu
%   x.T \isOk$. But is derivable by the initial assumption and
%   weakening.

%   The other case is a different $\mu$ operators in a judgment $\GEnv,
%   x:\gamma, \GEnv' \vdash \mu x'. T' \isOk$. Inversion yields $\GEnv,
%   x:\gamma, \GEnv', x':\gamma' \vdash T' \isOk$ and $(\GEnv,
%   x:\gamma, \GEnv') \setminus x' \Contr T' : \gamma'$. The first part
%   can be handled by induction, but the second part requires an
%   auxiliary induction to prove that $(\GEnv, \GEnv') \setminus x'
%   \Contr T'[\mu x.T/x] : \gamma'$. For this auxiliary induction it is
%   sufficient to observe that a successful derivation never reaches a
%   recursion variable, so the unrolling does not matter. 
% \end{proof}

% END - Moved to the section on types 



% PJT: Too limiting, unfortunately.
% This definition rules out \mu x.(!B;x)  --- which should be allowed.
% This definition admits \mu x.(\mu x.skip; x) --- which should not be
% allowed.
% \begin{itemize}
% \item $T$ has one of the forms $T \rightarrow T$, $T \multimap T$, $T
%   \otimes T$, $[l_i\colon T_i]$, $B$, $\skipk$, $!B$, $?B$,
%   $\oplus\{l_i\colon S_i\}$, $\&\{l_i\colon S_i\}$, or $\alpha$.
% \item $T$ has the form $S_1;S_2$ with both $S_1$ and $S_2$ contractive
%   in~$x$.
% \item $T$ has the form $\mu x'.T'$ with either $x=x'$ or $T'$
%   contractive in~$x$.
% \end{itemize}

% TYPE EQUIVALENCE 

%\input{type-simulation}

% \input{fig-type-equivalence}
% Type equivalence in Figure~\ref{fig:type-equivalence}.
% Given a binary relation $R$,  we write
% $R^{-1}$ for the inverse relation $\{(y,x)\mid (x,y)\in R\}$ and $R^s$
% for the symmetric closure $R \cup R^{-1}$.

% ALGORITHMIC TYPE EQUIVALENCE  _ DEPRECATED

% \input{proofs-type-equivalence}

% \input{fig-algorithmic-type-equiv}

% Algorithmic type equivalence is in
% Figure~\ref{fig:alg-type-equiv}.
% %
% For example, letting $S_1= \mu x.!B;x$, $S_2 = \;!B;S_2'$, and
% $S_2' = \mu y.((\skipk;!B);y)$, for an initial goal of the form
% $\cdot \vdash S_1 \equiv S_2$, a run of the algorithm generates the
% following subgoals:
% %
% \begin{align*}
%   S_1 \equiv S_2 \vdash& \;!B;S_1 \equiv \;!B;S_2'
%   \\
%   S_1 \equiv S_2 \vdash&\; S_1\equiv S_2'
%   \\
%   S_1 \equiv S_2, S_1\equiv S_2' \vdash& \;!B;S_1 \equiv !B;(\skipk;S_2')
%   \\
%   S_1 \equiv S_2, S_1\equiv S_2' \vdash& S_1 \equiv \skipk;S_2'
%   \\
%   S_1 \equiv S_2, S_1\equiv S_2', !B;S_1 \equiv \skipk;S_2' \vdash& \;!B;S_1 \equiv \;!B;(\skipk;S_2')
%   \\
%   S_1 \equiv S_2, S_1\equiv S_2', !B;S_1 \equiv \skipk;S_2' \vdash& S_1 \equiv \skipk;S_2'
% \end{align*}

% \input{proofs-alg-type-equivalence}

% END OF DEPRECATED

% THE UN PREDs

The $\un(T)$ predicate for types is an abbreviation of judgment
$\vdash T :: \kind^\Linear$. For contexts, predicate
$\un(x_1\colon T_1,\dots, x_n\colon T_n)$ is true when all
$\un(T_1),\dots,\un(T_n)$ hold.

% The $\un$ predicate is true of the following session types and
% types. The predicate is not defined for type schema.
% %
% \begin{gather*}
%   \un(\skipk) \quad \un(S_1;S_2) \text{ if } \un(S_1) \text{ and } \un(S_2) 
%   \\
%   \un (\unitk)
%   \quad
%   \un([l_i\colon T_i]) \text{ if } \un(T_1) \text{ for all } i
%   \\ \un(T_1\rightarrow T_2)
%   \quad \un(B) \quad \un(\mu x.T) \text{ if } \un(T) \quad \un (x)
% \end{gather*}

% DUALITY

The duality function on session types, $\dual S$, is defined as
follows.
%
\begin{gather*}
  \dual\skipk = \skipk
  \qquad
  \dual{!B} = \;?B
  \qquad
  \dual{?B} = \;!B
  \qquad
  \dual{S_1;S_2} = \dual{S_1};\dual{S_2}
  \\
  \dual{\&\{l_i\colon S_i\}} = \oplus\{l_i\colon \dual{S_i}\}
  \qquad
  \dual{\oplus\{l_i\colon S_i\}} = \&\{l_i\colon \dual{S_i}\}
  \\
  \dual{\mu x.S} = \mu x.\dual S
  \qquad
  \dual x = x
\end{gather*}
%
This simple definition is justified by the fact that the types we
consider are first order, hence the complication known to arise
in presence of higher-order
recursion~\cite{bernardi.hennessy:using-contracts-model-session-types}
does occur.

To check whether $S_1$ is dual to $S_2$ we compute $S_3 = \dual{S_1}$
and check $S_2$ and $S_3$ for equivalence.
%
Duality is clearly an involution ($\dual{\dual S} = S$), hence we can
alternatively compute $\dual{S_2}$ and check that $S_1$ is equivalent
to $\dual{S_2}$.
%
For example, to check that $!B;\mu x.(\skipk;!B;x)$ is dual to
$\mu y.(?B;y)$, we compute $\dual{\mu y.(?B;y)}$ to obtain
$\mu y.(!B;y)$, and check that this type is equivalent to
$!B;\mu x.(\skipk;!B;x)$.

% Let $\rho$ denote a map from recursion variables~$x$ into session
% types~$S$, where $\varepsilon$ is empty map, and $\rho[x\mapsto S]$ is
% map extension. The substitution of recursion variable~$x$ by $S'$ in
% $S$, notation $S\subs{S'}x$, is defined appropriately. The
% \emph{duality function} on session types is defined as
% %
% \begin{equation*}
% \dual S = \dualof(S,\varepsilon)
% \end{equation*}
% where
% %
% \begin{align*}
%   \dualof(\skipk,\rho) &= \skipk\\
%   \dualof(!B,\rho) &=\: ?(B\rho)\\
%   \dualof(?B,\rho) &=\: !(B\rho)\\
%   \dualof(S_1;S_2,\rho) &= \dualof(S_1,\rho); \dualof(S_2,\rho)\\
%   \dualof(\oplus\{l_i\colon S_i\}, \rho) &= \&\{l_i\colon \dualof(S_i,\rho)\}\\
%   \dualof(\&\{l_i\colon S_i\}, \rho) &= \oplus\{l_i\colon \dualof(S_i,\rho)\}\\
%   \dualof(\mu x.S, \rho) &= \mu x.\dualof(S, \rho[x\mapsto \mu x.S])
% \end{align*}

% This definition fixes an issue with the (flawed) more conventional
% syntactic definition of
% duality~\cite{bernardi.hennessy:using-contracts-model-session-types}.
% Example. Take $S = \mu x.\mu y.?y;x$. Then
% $\dual T = \mu x \mu y.!(\mu y.?y.S);x$, as opposed to
% $\mu x.\mu y.!y;x$.

% TYPING

\input{fig:typing}

Typing rules in Figure~\ref{fig:typing}.
%
The usual derived rules.
%
\begin{gather*}
  \frac{
    \Gamma_1 \vdash e_1:T_1
    \quad
    \Gamma_2,x:T_1 \vdash e_2:T_2
  }{
    \Gamma_1,\Gamma_2 \vdash \letin x {e_1}{e_2} : T_2
  }
\\
  \frac{
    \Gamma_1 \vdash e_1:T_1
    \quad
    \Gamma_2 \vdash e_2:T_2
    \quad
    \un(T_1)
  }{
    \Gamma_1,\Gamma_2  \vdash e_1;e_2 : T_2
  }
\end{gather*}

% EXAMPLES

\subsection{Examples}
\label{sec:examples}

Streaming a tree on a channel, back and forth.

\lstinputlisting{tree.cfs}

A trace of the type of the channel in function \lstinline|transform|,
where \lstinline|TC| abbreviates \lstinline|TreeChannel|.

\lstinputlisting{trace.cfs}

The arithmetic expression server.

\lstinputlisting{arithmetic-server.cfs}

Now with a datatype to simplify the client's life.

\lstinputlisting{arithmetic-server-data.cfs}

Let's try a SePi-like version. All we need are context-free session
types and type schemas\footnote{Predicative Polymorphism in
  pi-Calculus. Vasco Thudichum Vasconcelos. In 6th Parallel
  Architectures and Languages Europe, volume 817 of LNCS, pages
  425--437. Springer, 1994.}. We witness the extra ``plumbing''
typical of the pi-calculus, but otherwise very little (type only, in
fact) extra basic notions.

\lstinputlisting{arithmetic-server-sepi.cfs}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                 doc/ICFP2016/icfp16-reviews.txt                                                                     0000664 0001750 0001750 00000040641 13216017506 016256  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               ===========================================================================
                           ICFP 2016 Review #48A
---------------------------------------------------------------------------
               Paper #48: Towards Context-Free Session Types
---------------------------------------------------------------------------

                      Overall merit: B. OK paper, but I will not champion
                                        it
                         Confidence: Y. I am knowledgeable in this area,
                                        but not an expert

                         ===== Paper summary =====

Traditional session types are related to regular languages, because the types are tail-recursive, analogous to regular grammars in which the r.h.s. of a production must begin with a terminal symbol; such session types can model serialization of list-like data structures.  The paper envisions session types that are related to context-free languages.  Thus, the resulting session types can model serialization of more complicated data structures, such as trees.

This is a really interesting idea.  My first reaction was extremely positive.  I was briefly disturbed by the paper seeming to contradict the known result that equivalence of CFGs is undecidable, but it appears that the "guardedness" of basic process algebras is allowing the authors to avoid that, so it seems okay (but I think this point should be emphasized in the paper).  The first two sections of the paper seem like they should be understandable even with only a passing knowledge of session types.

However, I have some concerns about later parts of the paper.  The broadest concern is that, while the core of the paper is (quite reasonably) devoted to type equivalence, the linear type system is an equally important component.  I have a specific question about how typing for -> and -o works (see below under Sec. 4), but another concern is that the proofs in 4.4 (in contrast, I think, to the proofs earlier in the paper) seem very terse.  In an ordinary (nonlinear) type system, substitution lemmas need minimal justification; in a linear type system, they are not standard and deserve more, I think, than a casual "By induction".  Perhaps there are fuller proofs elsewhere for very similar systems, but in that case, the paper should include clear references to them.  It's fine to reuse rules from earlier systems, but then the explanation of the system (and/or metatheory) should clearly reference the earlier work.

                      ===== Comments for author =====

Sec. 1

"recursive datatypes and XML documents": an XML document is essentially a recursive datatype, yes?  You don't seem to return to this later, so I would just drop "and XML documents".

The mention of polymorphic recursion seemed a little premature.  There are plenty of type systems that allow it (DML, other refinement types, etc.) and it's only a concern for type inference, which seems off in the future for this system.

"moniodal"

"functional sublanguage": define here what this means.  It only became clear to me near the end of the paper.

Sec. 2

"given that c :: TreeChannel;\alpha": Use ":" not "::", for consistency with rest of paper.

"equi-recursive...": Define and/or cite.  Many readers won't know what this means.

Sec. 3

In Fig. 3, note what star and 'a' range over (it is explained in the text, but not the figure).

Lemma 3.3 is stated without any proof (maybe it's clear to the authors, but I couldn't see immediately how to do it).

"constituend" -> constituent
"the exhibit" -> they exhibit

Sec. 4

"fo expressions"

I'm confused about the rules for -> and -o.  You treat T1 -> T2 as a refinement of T1 -o T2 in which (for ->) you have the additional information that the context Γ is unrestricted (the second premise of the intro rule for ->).  I thought that the intuitive understanding of -> vs. -o is that -o cannot use its argument more than once.  But the difference between the two intro rules has nothing to do with T1; rather, the difference is in the rest of the context.  One thought I had is that "un(Γ)" might be a typo for "un(Γ,a:T1)"?  I checked Gay and Vasconcelos (2010), and they have exactly the same intro rules, but I couldn't find a clarification of this particular point either.

The shape of a possible counterexample might be

  λx.(x,x)

which (using the copy rule) has type $T \rightarrow (T \otimes T)$ under empty Γ.  Then I use the rule that turns → into -o, and apply it to a linear type.  Hopefully I am just missing something, and the authors can clarify.

Please add the names "weak", "copy", "~" that you use in the text to those rules in Fig. 6.

"fat rule": I have never heard this term.  I would say "combined rule".

Sec. 5
"et at."

===========================================================================
                           ICFP 2016 Review #48B
---------------------------------------------------------------------------
               Paper #48: Towards Context-Free Session Types
---------------------------------------------------------------------------

                      Overall merit: A. Good paper, I will champion it
                         Confidence: Y. I am knowledgeable in this area,
                                        but not an expert

                         ===== Paper summary =====

The paper extends session types with the ability to express sending
and receiving context-free recursive structures (i.e., trees). This
requires non-trivial additions to the language of session types, such
as sequential composition of session type expressions, polymorphic
session type variables, and polymorphic recursion. Neither of these
have been considered before. Also required is a non-trivial extension
to type equality, which now has to include equi-recursive treatment of
type recursion, distribution of sequential type composition over
branching types, and monoidal laws for sequential composition of
session types.

It is not obvious in this setting that type equality is decidable,
because one can write a recursive session type expression that is not
tail recursive, and whose unfolding goes through infinitely many
states. This contrasts with regular (i.e., tale-recursive) session types, whose unfoldings are guaranteed to cycle.

Nevertheless, type equality is actually decidable, and one of the main
results of the paper is the decision algorithm that works by reducing
type equality to bisimilarity for guarded processes in Basic Process
Algebras (BPA). The reduction works thanks to various (reasonable, I
think) restrictions imposed on session type through the kinding
judgments, such as productivity, contractivity and first-order
recursion (i.e., no sending channels over channels).

The paper then shows a form of a linear type system for the calculus,
and proves the appropriate forms of preservation and progress
theorems.

                      ===== Comments for author =====

I am not an expert on session types, so I couldn't really determine
the difficulty and significance of the main accomplishments. In
particular, is the decidability of the type equality in this context
surprising, is the reduction to BPAs unusual, and are the restrictions
on type formation that are needed for the reduction to work
reasonable.

But, putting those aside, I really have nothing to object to in this
paper, and I think it should be accepted. The idea to target
context-free recursive structures with session types is certainly
well-motivated, the formalism looks very natural, and the paper is
excellently written.

One question that perhaps the authors could answer in the rebuttal
period is the status of Corollary 4.11. It says the progress is proved
for the functional sub-language, and I'm curious what would progress
for the full language look like (if that's just Theorem 4.10, why is
the corollary even interesting enough to be named?)

===========================================================================
                           ICFP 2016 Review #48C
---------------------------------------------------------------------------
               Paper #48: Towards Context-Free Session Types
---------------------------------------------------------------------------

                      Overall merit: B. OK paper, but I will not champion
                                        it
                         Confidence: Y. I am knowledgeable in this area,
                                        but not an expert

                         ===== Paper summary =====

This paper studies context free session types in the context of inductive data
type serialization. It presents a syntax of such session types and shows
examples programs operating on binary trees that they are rejected by type
systems of regular session types, but they could be potentially type-checked
with with a type system of context-free session types. The paper makes progress
towards such a more advanced session type system by proving that, given a simple
transition system, bisimulation of context free session types reduces to
bisimulation of context free processes, which is decidable.
A type assignment system is then defined which, however, does not give rise to a
type-checking algorithm. As stated in the conclusions, more work is required for
a type-checking algorithm for context-free session types to be possible.

                      ===== Comments for author =====

This work is a step into an interesting direction: the improvement of the
expressive power of session types. As the paper explains, this could be achieved
by typing programs with a type system supporting context-free session types.
This however requires---at least---decidability of type equivalence.
To achieve this the paper translates session types to first order processes and
uses the known result that strong bisimulation for such processes is decidable.
This is possible only after dropping the usual delegation construct of session
types, which would allow channels to be sent over other channels. If this
construct is added the session type language becomes higher-order and the
decidability result does not apply.

This is a reasonable simplification for a first study of context free session
types. There are, however, other decidability results for strong bisimulation of
the higher-order pi calculus which could perhaps support full context free
session types with delegation. For example:

  I. Lanese, J. A. Pérez, D. Sangiorgi, A. Schmitt, 
  On the expressiveness and decidability of higher-order process calculi,
  Information and Computation 2011.

Ideally, the restriction to first-order session types would lead to type
checking context-free session types in functional programs. However, the
conclusions state:

  "There is more work to do towards a practical type-checking algorithm as the
  algorithm resulting from the decidability proof is very hard to implement. We
  plan to proceed in this direction by giving up on completeness for type
  checking, but going for a sound approximation instead."

Does this mean that the only challenge for a type-checking algorithm is a
practical algorithm for deciding strong bisimulation? 

It is difficult to see an obvious answer by observing the fairly complex
inference rules of figure 6 (the type assignment system), and the paper itself
does not address this question. Even the challenge of finding a bisimulation
algorithm is not mentioned before the conclusions. 

If the answer to this question is positive then perhaps a type-checking
algorithm could have been given, assuming an oracle that decides bisimulation.
This would at least have shown that no other complications exist in
type-checking, and the source of potential incompleteness is only due to type
equivalence. Moreover, one would be able to plug-in in the type checker sound,
decidable approximations of the oracle and investigate their usefulness in
actual code. 

If, on the other hand, other substantial complications exist in developing a
type-checking algorithm, which have to be addressed in a future paper, then a
good discussion about them in the main body of the paper is warranted.

===========================================================================
                           ICFP 2016 Review #48D
---------------------------------------------------------------------------
               Paper #48: Towards Context-Free Session Types
---------------------------------------------------------------------------

                      Overall merit: B. OK paper, but I will not champion
                                        it
                         Confidence: X. I am an expert in this area

                         ===== Paper summary =====

I have mixed feelings about this paper, but I am minded to recommend accepting it. It addresses the issue of how to express and enforce communication protocols which trade in tree-structured data. The goal is certainly one that I find motivating. The complexity of the approach, however, makes me rather worried and inclined to ask whether we could live with less. Specifically, the choice to use an equirecursive fixpoint is what makes type equivalence here such a hard problem: it is impressive that they solved the problem (by translating types to processes in a calculus for which bisimulation is decidable), but is it worth it? The decidability property seems brittle and reliant on going not much further than context-free data. If we want more expressivity (say, Haskell-style GADTs), this approach to type equivalence will stop working. Even so, it is good to see how much is possible in this direction. The paper is well written, has good motivating examples, and has convinced me of its main results. It is a valuable contribution.

                      ===== Comments for author =====

Section by section

1 It is good to see sendTree, which is clearly the simplest nontrivial example. It raises an obvious naive question, however: why is the type not something like Tree -> TreeChannel -> end? What is it that makes the polymorphic recursion necessary? It's a little sad that the discussion of Sill comes at the end of the paper rather than here, because the readers really should know that somebody already has a session type system which allows the communication of trees. What is the type of sendTree in Sill? It seems to me that the job done by semicolon here is done by the tensor in the Caires-Pfenning school, but in a rather different way.

2 This section is very helpful. It is good to lead with such thoughtfully presented examples.

3 This is where the hard work happens, much of which is the consequence of equirecursion. Is there an isorecursive presentation? I would be tempted to start with explicit fixpoint unrolling in the language, to simplify typechecking, but retain the contractivity requirement so that every well typed isorecursive program can be translated to its equirecursive counterpart. We then would not need an algorithm to decide type equivalence for equirecursion, just a theorem about assignability of types.

  p5  the exhibit --> they exhibit

4 Am I right in noting that some let..in constructions are just syntactic sugar, whilst others are eliminations for tensors? It might be helpful to separate these notations. Also, I worry when I see let..in syntax immediately followed by in as injection.

  p10 rules fo expressions --> rules for expressions

Let me remark, though, that the GV-style does lead to very readable programs. This part of the paper makes a valuable design contribution which would survive any reconsideration of the equirecursion issue.

5 In the comparison with Sill etc, it's right to note that the CP way is explosive in channel creation, which is fundamental to the treatment of the tensor, but is also the way those systems achieve their compositionality. If you implement such systems naively, they will indeed have a heavy channel creation overhead and issues of embarrassing parallelism. The semicolon here is a more overtly sequential construct than the tensor, makes it clear when we can use the same channel to send messages in order. Amusingly, though, the CP approach might lead to highly efficient *parallel* communication of tree-like data, exploiting multiple physical channels if they are available.

6 I am glad to see that the authors, too, recognize that the complexity of their system is a considerable burden. The part of that complexity that is needed to tame equirecursion seems hard to sustain, and worse, limits the potential of the system to advance towards more subtle and flexible typing disciplines. However, it is certainly good to have such a thorough exploration of this part of the design space.
                                                                                               doc/ICFP2016/icfp2016-paper48.pdf                                                                   0000664 0001750 0001750 00001233716 13216017506 016161  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               %PDF-1.5
%����
3 0 obj <<
/Length 5210      
/Filter /FlateDecode
>>
stream
x��<�n�6����ˉp+�H5��"	6{�p0�ه�<�jڭI�ԑ��:_�U,R���%9� �b6�/ź�hvuŮ��P~s�����EZJ��n�i��U�X�
~u�����lJ�tΒ������Y�m׎�����~�ubAߛah��*7�"KOf�|�����45K�\�8����ν-u������a�z�!�y��R�?`��Lˬ���Թ��h���e�3C�7��`�s=�{��ֺ;�mSW��-�e�7��{�Źq���4Yk��px�l��d;�Y﫶5��j��J*����~�F����'��fo��X5�mo�s?4sGj�6YUs<u���+��HN}7vuw�ڸ�5���Z�x<��q1�k�?������?W�&�vX}� Ar� �<��T	�f;�qĉ��g�au8SG��7԰�+���]��{����Lxl�{j�;�u��l+ĀӺ��*��ї�il�� hg�@��>�u0}S�_��}`�x�����Ȯ+�eQ��O8�`�/v`��w�#�e9���|��k����ݹ~��]e�����7����El#�h�`�-�l�+�!��_�3`��s��웚����s� b	�ȽkSjyGs]��O�h���=~�p�"cvd��{�M�cSSS}�D��	bH���s���P8�pT�?w�� X�H>��D�m�҃����F��<R�����T��d<�7����H�$h�D����w�ۅ��&O�N"�:~��.�V�^���;s@�ۡ�3T}@R=nT�<I���hF�� ���C3X���> ~~�J�?Rݮ����g�X���=IF���S4OT-�"8Yj������Mkzġ���v��j?����O� �u�tf��/Q[�v$ ��G�P��D?�~Jd����K�m�Nj����W�7��$:�v�H尟ŀ�5��}zI�槳SRV���v��Nƙ���A��)�l���T�9��<> �;\���ϲ4+$|H�?������{0Ǫ��u�8��6Nc/�7:p^*TIsp��1�������p֧��2J�,���BΟ�:@n�q�v�P�9���P�c#9 ]��W}����z4�j'�pyr��(K�Ua�^�+��I XUά
�avO ����Hdak^m#�<�\B��l�E��k�s�1%ؿ��������@�:��c�lH�Y�p�ZV���r����x��h]PJ�iG���{*?p��~����g�piv1���^)&Ӯ�@�?��As:4���u=�pφ�,�ho�]����D��÷H *1U�b6G�:�`ܖ M�#novgkRl��w�ܳ�"Ǳ7��S�3��J��O��@���/�A��}��C�q���rL� ��/؆	��Á3-��뼱��*�ٛ���ƻ��M<��E�=4��}5)�B�&����H=�����[<g�Aj�*����^F.�|/P�Jow^��%�[���X��,:�.$uɃ.e*��r���?FF�!4�w��{ףz��X���'ׁ�����_Ŋ� ��|P^���&:@��/b��iQ��5�i�����T�䭧 �漰*,˗HϟB�|r�ɓ3��k����{�Ѧ!���.wB������Θ|a�9���\��:�{��'2P���r]�b�w8��-�Ba�Z�`Ք���ed)�@/s��Qc$f��T�!:U�#�r�A���H�e��P�-��c���0��V���bTZ�Y��OdE��8�l�<�r8UY�����Î>����._B�x	�r����/Q�R�V�����C	�.�|Gʭ�*�ӗ_���<���k+�=����ʊ,1Z:4tE�9rSdOMW
���Ӂ��S�k��y���F۲�̳��V��8�&� �u"�_���P��X�k��������b��T*��K�E��D���,\��v�Жk���m�0"�a`��')Uo�@ �����,~7wh#��4��ho��2<8[�p3�X+�%�e���sy�l��Y���f|M�������B�iZjk��u���5bژ��,�Бk��P-�$1�[�9+^1�58���1��9������v�R���η,���јǫ����TEte�z��V>+:�B�4���O�F��6�մg
2jg�G��P�/��,�X��j0�m_����x9��?v���a���Ȋ)��,V�|
�W[1�sD�y���Z�@Ny�D�7tD*����Qi�_oZ.E�Y�8/|P�恄�<� ҆�֣�������5U\����T`�z����L�o;Е����<���Gٻ�����&�m́?�wa7�:`H����|����ҀUv`i�6�fl�F�u��̖��Q�[�zM2+v��~�f��d���e*#�f
����`~:�	k�DɄ����x@�wK_���5(@����[�l�R�|�J�����G{c�ٍ���?4��C��d|Ȉ��b{�	��ԛSߴ㗮�4N$KЛc�ޥ�I�I9US�[RV�FbrQ����͖����vCE��E)�Y�&@��ZӮG� ��l�p����T��M�`�xW¤]	�v%,KLd	�n�
U,��+J�L�k;0&j`QR��o����Z�(�xҺL��q^�:�-��֩��A_DF��T���`W0�+�@��|u��m���<����.�����姥H��V	��hW��ųT]�	���Ⱘ�T����<��#�e�x�A��>�S��M�w 8J4������.4�����(�<�$!(A���)��p�Ql����D0z��J�T�9��az"��zY��N�;����`g#&
�ѯ�%�p[�L��2���%t���������
�l�p��	!d ��!P��+ق�!���/��#lڜ�\/����'ӥB�V��J��
�@Z��60�E�#�͖+�˟o���M;vů$����\�qY�����Gv��F�5x��Ճ�z��L���޿��)�.�O3��W9e�O���/���%7̓�"xg�\����ٺqf�m�V6C�3̆��ӫm��MI �L��?%QT���\���۟��ք�{����yQ�\D�v'$T�]d.����|�3����7D�%)13��b-�Y���1I��I��L����;Y"B8_�,�����=9VNZ�������K�'~��{
��3 V�p!����h��A�ډ��9�,�#�t�,���+f�bc�ۦ/lv�TumN��_c]�S��1�1U^��:��f�f
T,R�l�.�e9�Po������U �%Ta�o��(�y��-��x'q�|bJ��ܟ����;��x�� x�%���(:�\F)JS��8��(Ea����`��KV���;P��y7f��\h0��U�����Rfs^V��R*�:!e���c��Gr�<�Z^�W��"�J1��1;r�u��ml�C3�KUI�M`P<�J/�X���0�K�/�D_Tx�;���&�ə��|F���G��,����
���E�ϛ~�r0*:�mȳ��,dsnM���$�=�r �g�ڞ�v]mA1��]���,�hZu�)׉� !�U�O�ʔ��Ee}�̦GuC��j��O+w����J��cu�5�O	�#k�,{�r���;c3�pzJ.���S��ahƳݛM1�v���c�ʗ7aG�e�R2Y`x�9/	s�r���d�洉j��)��H��O�c��������Q�s����ݏ(�Yؙ��Y���`n�cS���A}L#��WJ/3a.�D�y��ys�4?�6Ӎ���jăA��96�#.ۿ��hz��ٖj�����O	^.K�����X�@��]��؋/���@Ǧ�C��aD�j�+v<þ: �ה�j��*�삄_��f�2�Y�"���YR�����E��� ���Ƭ�ȑ>�}n�j4�y�ɚ'/1`�'�7㗛<O<M�&�=�y�R0f{pG��)#�lW>���3ن�iѢ#I���?�w-y����U��kU��Qy�v~�%Ŭ�ْ��v��?�i��e��@��?b�M�?PKk(�Y���I��~��n��Ɔ� ��e�[M6ך�o
0[ڢ��>�T�Kj��"��.�q�z�#���ޔz����%*�~����Jl�^���?�MqG��5�����WO�� &D��e$���:�tǏpqq��b��	R��-�%'!
��j��\J�R���Љ��xR�Ny�&4K~,�����Ta�aJ#�s�Ķ2�O���*nS$f�:B&K5���CDd:`CLD��@��P~��Om�����&��C9���s.��*ʘ�!��\��'@/��k�s��q�éf �s��cU�N��(b,k��rϿ��ٳ����"\>*��I�#�jX��<KNT3�ȫ�yx��/i2����yH g��
�G�q��ϖ��" �z&�X<:�s(����W����G��-��;���ޥ�͊4ǔ��3�5�{=5�UW%z�� (��&F!}M��~~M�j ��\+U����r��=Ұ� Y���'�
攣�Mu�T�LQѐ偠�7�LﬆIv�B4x��9���Q�r�q��lp�+�ţ]�QeK�3�_u������N�^��$�@v������L+�JZ�X	��H?�r���}�p��OE�ɜ� �ũ7qG��4�`u�.�Й��b�r�*��X=Ȋ0�Ϭ�H�H�K�O.߭FV�:�0�.G5�uH�H�*��.�ҳT�Id�I����g���x��O���D< a���;�%���295O��AF ����(���r��e$8j�
����c��Svk�w-��kw ��1~��L)�bg��Ciz���U�$W�M�,O�\����ݓYMR�6"!�����S���V�]u�^A�T��? h�4�q��x:��	K�׬ȂL�)�6���a����X�.�>9�wVjl\����hˇpr�~T��q,�2<����9���@��������:�wS��t��
���^%�1`�����`���?|Y�������k�מ�'ܣ7��;1G��_������s�;��u>�Ynv�˞L7B]�t*�Tk�R�OAȹ#�	�A���J~����� _[�
endstream
endobj
22 0 obj <<
/Length 6530      
/Filter /FlateDecode
>>
stream
x��<�n䶒����np+�M�	�Ip��"��,2�.�X�[�uF-u$u���o�(�jz��d�<��&Y�x-֝�^�^�߼J9��ݫϿ��E��Y�/��\}�����������꺺M.�*5�ww�������M�}���h	��/ƺ�S��_)�e�Q��jj�������"��#+J~���oO5�M���<�+*���x�F�b��D��Ŷ��y!�v#�����[��t��
sso�����z�dz8rnln�
z��M?�vZ)MH҄���������};]ne�Ӝd��vWM��I/��S�{�/L�Ȼˢ�����tuRJ%yV��oNݎ
G��f�<�uͳ���	�p��R���6㴥�"OL!�6�-49�oa�-m����NmC�?���B\H(�R��V
;�z�)3�5#rϠ�z��_q�u�px���S�*۴Uw{�nk��b�]8T^?�Zw}7�á骖H����L2���E�H�s��n,��SjR?�FiF�p4���i"D��7p���Fm�/e�Al=Pe4�vq{j��ҘMB�o;Jq��'2˾`��k}8��fG���`�Le�
/��}=��7S��n&�+*¨�[�VZ8#,�Dv�@��D����@#���<������~��蘏,ǡ�5c�>\ff�ۣs�h��.��~JS�5SMutR�v'����S�j��O��}���Z�[q���H<�M��H�P���X�szǭ�ܧx��V�eK,��	تۻL��J�t���"�%)�>n����R�`J�>�!��+�p�N^�;(�Wix:P1ֿ��ngyT�f������B�Rn�����Fn����d�a3p<	ս!�����,�P"O�a��m�ЏmS�N�L�  �J$��-CpEE�� ��o��p�s����T5����q��p���wb�ͺ���*	d9�͜����8j?�� �Թt���W���c�������{K0��?����"iѥY��b3\"�ۊ��It�����Ɍ�R��*�C�,KF�9�8�gX��uM�WM�Z�N�ؠ����7CX���,����"�6�H+3�����@-M�P��r�{�T�a9�H%'*eD�3K�q�@V��܍w~�K7��݂7C}l��&]���uޟ���]�:F��P�jB'��}F"M�́�̬������2�3��{d,���p��e���|����$B��l�!�����#�K��X�ț�t���\���-�<�9��iR���Md:���Q��L�)�b��P� :T��@!bit�	W�ǺRi�4�'$��x�62�,�K�O�(mg@�Ez�i�&*%�Tܞ�'�V��mz�MWOm��Цo�)�T���˱�腠��R���
�O������7�
j���Hg�O�¬{K�������2�=1��5N
5O��(�Y�I�[���e#��LI-ߑ�%P�aá>�Ƚf�`DڡJ1MwrrTw���b�5\����r�c"׮�eA�9}��j�'��$��
NΤd&���_޽�����B�-��T	Ї��Տ?�{��y��..�m�";E{����$�NpTR�Ȩ�Lx'�/�����B��e�e8�}3���A|�"H�T�U~���n����Fɢ2���wwV1�5㐎����#�a�6[�UX�z�1N��M�,��6�����z�i�(LiOn�[�0�SJ"8H(�H�s����B;�%���)AI)�`H1R(�0t�>���\P"�l�x���j���� ��U�c�*B������5.u���ZZ������w}���!�<堲���J)�4/����ŎR�m2�x�e��0�@/�b"���)��b�PI�˗�� 
^�PmBص�?Դ���m[��ҳ���+��ǚC=UP�ְ���6���5K���Z�N��an98:��E�2A��:-Q4@�ڗ*��5w�.	�B�5b�E05���o��>kF�J�$+D���n5Q��55��b֟������ȊY�X�vш���Ź��L�� UA-���u3X��I��}m-����IfP^�l'g���K�b^#�v� ��w{R����4jpF)]䨏�6�R��^�+@96��ho��2�Tg�̳de����� �n[�?V�+��W̽���~yEf�(�7�s"�ma�~��UM��`Փ	~��r��
okA���$-��#��G��͑J�c���^N���&x�DII����;J*���y�Al3P�Y�5�.+a8O~�D���pC�P���i�FY�#&h�H�S6��Qi��
��J7�)i(9��$)sV��#m�H������$�5֏�K��2{IS��v
v8#d��q����f��I��gP�J:�28�"8�28�"8��;%��,����Ϗ\��.D��97~��Oi��
������ʆ�j1�T��ʹ\���V�� ���u1r0��,_�.Cr-at��R��l��c%Y!�(A&�=0(AHJ@�בc0�9�w���k��|i WSE晁v�}��;�Tٙ�t9S=�
2��~h
�Y&��Ϳ5B�"�bS&�r�%��.�l?���C��{y��I��p�R[�5�H�n��Q�1b�ɚje�,�|[��J����,5���ډ��B��<�:N����X��Hr]��G�Z����k\�]�/�D-B�\vH�����E&�5N��`vɨUZ�~�w}��:�����:�B���L��$�)Ӌ�t�\�
���
M������S���5�X��>g �e��"��9{B�/� : v�a�	��������p�Ќ�R��k�G����č��3�k;{Q��2��:��&|�����}�p�#hB�+�	Qٖpyƺ�X��B�BfuE5> �k�;�ug{ᰊegn��UJ)m�ud|��R�1�]}�=�z�#V`e�3�͸;��p�F��H�ٹ|n{p�;tZQ��y��6���XW�H�_N䠅��.�цbJ������p��c�<��g�����<��7�� �.���� @��E匓��Q���ۇ��p�QhGr�7�`�U+��zw�R5]�p"�"���<b+����f��:�Hu��:�[ ��u��$�D-���"����>��e@���?�fS��6k/���[V
��1"��B����g1YFZ�Mo$��f~}�S`a�NV|V��n�$"AZ>��0#D<f��>��s��f�f���&udOZ萃|_�(4]l$G@�P9׬�x,`�,�rA��Qh�4��d}{����h�+ec����#c)f��6�}�Ў#��D���9����8�k��2
���Z�.�Z��R'�8��66��(d&ut��4��(}�t��|5E�|25I��^�n�*g�^�ɮ��Ce�j�yvmE�O��劲c�ZVQ�:M����*��GP��a���85��f`��;D�
@ʵ]��:�:�z8V�t����]���<o	(��4�*I+ݜFV��=�Ӏ�Z�5�QV5��[�O�
������H�O���4�R�V��ꔤ@�m˯0�a��b3�3�;Y1/E��٦��e�''R�)$��	j�� �x��R���(��ٿmd̮ߟvl���P)?*O�4��ˇ@��v�tlȐ�3�{�Y�Ԃn����=�EMb-ztx*X�ɮ:��D���P��v�nj�:���eq
	I���WBYS쥘%T�\����_��̾ߝ0Fڒ2(��AvO,�<N��$��8�/�:�p�Y�".�u�l���g�=�H�� v�gK�����a.��ԃ��������/��z���.�Ι���67����2���Xx4�$\vz����Pۗl��6�=ǋ���o�	�+c(`�&4��AϯE�FⳳM�z2�$J`�?i����"�چe|���l%3��	�f�3R���.;�ul�̿�l���Ba᳇q���9e��klBz�oO�P��N�Dn��Y�{$C_,��9j�>���^��ΉZG�z�Y{��^�=~`'�>���֓9>])�Sr��wMO~ge�KE���~�l˰l�1�,�Y�k:OD�>�6) In.�B���)�d�
׏����Q/1�J`}yQ�G��x�Q��R~*�T�2� �v�ȝ2[c���P�T,�Mˎ;��"��`��pM�W��9b��YJ%3�,�IP�r�A��W�M�L���_K��aM,,�x|��<&a��H����jt���X�jo��
�+�n�"��Đu2�<�����a��(y�є,�K>=�2�f��@9/
JUۓO�p�!'�=��	mʧ���CW �P�ƞ�d���8����}ן0~!l�����Ng��H\��A@��z�IH���Rࣲh�n��_	�6��z�K�,��/q:�SDv���C/It�;T�6�Ԥ��@zV�(�ϔ[dX~��@�`�EĢU�L�VG@Xg7Q�3z�C��V�,��B�Y��u'��'5��uیwK��nO�Hg|:+��3�n��8F�B8J�V /u�P�K�aGHM�f�+��P �����y�
�@���k�~˾x#�βE�����H�-��M0{Q2W��b���/�.:Z�P�Fn�0����EH=!6ݘ����=��E��Z��$V��/Вe8�Ab�T��V��)�	ݗu�G��zgSy1F��S�E�.f��z,�0h�D�.�:Yh'.;Ѹ�?k�rmh[��ゞ�EM\��5H����]�yθ|����k�'7S/h�Hb��E������˜�L���=���R`d\��q"�.7���Ԏr���hu�\O鍳�b��7 b�+6����ڽ��G�W�O�	�>����i�oN��1�E?cƛ�Zm�R;����;���˰�f���D�ww��=�$��H1���~W��C�&�H��p��1.fl�4�G���L��i+r��W�k�͎˫0��P�].��!U���X�onC'g���B��^��[$,��(�ްo�1��L��<�q��AiGI̦�0fj��<3V��gj�t��j�)�m_)�1��1��@۟��C����kL���|~E~�X�J��>���{Ӂ��+�I�d��U��CE	kgW2(�b�][X!H.V�&~�usK
2�3zj��6�qy��=�Ծ���W�悥��-�S��.!Z>
IVJ�+��Ԟ�WR$�$x��K�yj��Fl$g����5����|Ϲo~@I8�"�S��}�2[��'��+|{}hSh�!g���Ho�t'~�5X���`��k���`*:��1���z�1X�{���O%ҟ�?�.]�^�u�����s�*r�}���h�� ����,��WF���Y�,�J�*���"D��1�[����-�������7e��-���\-
��^��f�/��_�̎>������]�I)�{WTG��}M'_@�t"s�kY̖�z7��T=T��X���à
dj�~M0�MȞ�	���h��Є0�ξʝ)?�e��_0�������(ᶘ�_jV���Y1snm��lˢ�,Q�(�l�%H���.��+���y�yz=��v�����C�0�8�E��γBtB~��O��Kb�=��>���C�o���7@�U��#["ǣ��������O�y3R��KpF�?&��;�󱾣���i����b՘
�L¨�Z�n�����	�ۄN�_�pN�j:��1��u�F�=�Z'&-Wo!h�$M����L5����4��z��؋�� ,�,�g��[�"��^~=�#��'y���	�G>��uY%0�E͉�A��.�� {�:m����;����@!|�K��b�}D&�èGn��������;�5d���؁Ġ ���?�O���[\��r�?!l�6!���s���@��lsCh̅ğٻ�J�O]\���f3�9��xA-O@��T���W��j��T����1�Fcc�W3��4A���.Jy��rv�:��+��Gxi��������=dB.�=6&z�1Az��=%H�<B���FO�^B�\�L&
��>�{�#*�OH��+*�5��ٴ�\�ݜF�7���\��^i��J���8⋀���ǉ���f�\-��������s<�����9��k��Cx:�N�R.��->4u�c��\֗�� j@P��h�87�E��`Qf�Wz��f�8~j9�otW���(6�+�GD�Tx��������Y������� ��󶺏��\��&��{�{����,}ęà� J�9�&�Q@��
VD�9��HzH�'�2�yxm�~�W��I�3x��]>��� �H�6��P���d̗���縰�#Zc���!]e�S��.�3c2О�Q�~�'���]�<)�K��b˟ ���E�\��-��S�!a��:�n��91�-���>W�t��w��_��
endstream
endobj
26 0 obj <<
/Length 6281      
/Filter /FlateDecode
>>
stream
x��<Y��Fz��+�/��u�hc8�8Xl�x�]`�l��͌$���g&�>�Q$��Ҍ�gf�Y,�����Í���W"����շ?[ySU)�͛�ggnJ�
�o�lo���ݮ����o�����}#��:R�����+y�vZ����C(W_N��t�=��i�EnLm
k���?ݮ�Xu�kS��t=����ӛ��*w��E�\<M��i�:�fUa�p=�u��|����y��B�B3�0���L�x&z���9C��$�R8 >m�x�E5�����Rb^)2�6�=��VzG�T	����2�JW�BV�(Y8��"�)���u*����Z��q��}�q�-l%����V���`֤ (��:��(����*���׫�r�!
�g���hO���<z���x����R����ٗ����t�Fc��Л��kZ��@���}��P�JOg(	��)?r���j~:$E��vם�-`�I(^�[/V���c�y�`����l��@K���]��X�b��S��c�p(Xy��c�7�20������<�����*�+�`�@
ﻞi�Է�f\C���e6|?�m����6ܰ���B����C76��p�=�F�.���4��<2�G���f�y;��ՌrE�׵���Χ��8/�J \��}�9N@Lit�+�&����(5���\_1�V�>�ï����;�����81�x'B�U'�O�������������*�^�A�����Fj#\9l�Az�k������Èwv��4��+�{���\��	��[���!"��	d,L!�kWa8�$���Сy��CT<�iwǞk��:`rY�C��L��_@��в|&q���n��� )� �~�~�a�qیu�{� O�ܞiT������B3����!]�>82j�쫟޼��+<tq�� z7����f��׿��-�i��p2����nv7���O����e!w���p�l<����l�AS�t<`8Ek�~Ѣ�y�%�)����8.�#iU�������m���tJ��#���5#(RR]��'�d�����P�,��U�E�Q,H���0�&>�O7��X���ݏ�a;>N3k��#5��8�C�0�a���߄�
l�u8`�Y�c�ʾGm�
#h��d���|Q��NvG�w�AY�b ��}6_QB����í]��)����]n�-׌� �M�����P���p�w�aӤ��M=ݦ��0B�oTN"�uhx8$aXR�� �p�FT��R/����a��$�Z��m�$B�H��е�z��:�L�5,���'t�߅���S,Y��e)�ɡ�����O�ԗ����1&U~���NK��\.��L�g��d7Pꨍ�Wb�!&�`�D87��	 �nל��c�~"���Ŕ/����w�?&F������w��3P)ANR��T����f��}a����ao�0c]�z�T�@�\�G�7>�ü�H��H����������G���y���j��~9j��{	��*�*��pb���	�<I�1�����\�ǅuC�L�B Vͨ����3�v�]���n���V��f�r=��ι�׀�s�-3`��d���=�7D&�	�w��&����i˛[���&@����aY����f������>��9�"�W�>7$���<�.�B��mw*��B.$�g����Z��Y$0��%3C�i&P�L�Y5�) �4����\��&��-�ʬ8�4`�8�s����ҝ�r����'֎=!�Ҩ\�r�F����f��X����i����sg*��@��TW�Ԉ� B$"J���X�.��]�W��c ���tL��`���%d�0x�ajA�ѧf�i��ꜰ��n^u���s�C>_U_��\d��l�\�B;yb�&8shߦC!I�2GC�	�P���u�
��;�|P4Ɲ��IO@��[1J3Jюa=�qV|�8�26��¡�$������|�(ԦI#E�̶%T��{�}�f�m߄�~BP��o���s�:�:����=��gZ�JU����'oq���wc� = x�Df&���4�6F��E{���/�����~_�n�EYM_1Ū��06{�eTԏ���Y�ǻ�}�L4���������%:u*�0��&(=Z�T��WϺC�~>�0�ڠ�
[�R�]��7�?Ѿ*%R,���Ϭ%3�ܓ���U���V���۷���%3/<��n���N��g6��3�DV<:.�ݶ`���f�v|�}MVF��:�2cM��@�י3!�����b�zP5L��}x9Kl�j
� -����BE�-�F���CU���ұ�bBժPej��M2�v)��w5�ɻ�R�U�A^_��f�x@���D��L)�cZ�݄+�f��vĜ���m6cƬ�9����K5�o�=�$�57z�o��Ц@���G���\�xrΏz�XP���]����@/~~���W�<(f�����x,��'4HS5Y��|l�����oG`�pt��'�T�I�*�5 �-J�'.��Ԥ��O�����W��1�Lሺ��zҳ��9uHw����~_��jS��TVA�[�<��QQ�<�{'�i+��	+���8s��4aY�"PŇ �tO��7�73=�F�6|K�7����s���M_Mx�Μ��n�����wz�e�kC��v2�*��e�.���f$eỼ����(�a��j[�u@Sx�.�{f�0�	+A�J`���i�5�<�w\�aQ>��Ц�����4�~�Z�5рʷ�6	[ �Q4(��p��$-��0�
�����]bd��G�y�xs� T�/$�F]a=S��N-�!v\;��T0(ߐ������ffxq��CY� ��&S���������Kb�����^f�/j.��w,*�Gy8@ێ��S�����}ڋ,td�?�u!����eQ.m,�;����ܵR�.�i�4Mݳ��n�m��v$�<����uY�J�e�c���,�A��Nt�'ʞ�dlo �G���k�k65�(>P*���'4��r"���N!QMj�9�Z~~ �г�#��5�$�>H�Z������[q7��K��ùz���ω����4��K͘B�Ჲ6�O�f�����f�	x�:T�	�UVW83��X��=)�42�G��ؠ&+�����<����/4E0���<lJ����2����ݑbB��Q����A�ǧ14ޢ큹qxA���Qۇ>���#hY�I4.H>��8]z����bW�o���OM>�U,��ܮ��@!�vt�麧�t}�kMWX���9CmEq�[��3��0���A��hL��BQsA!1&ҩEa��Cl/Vo�h�xʭ^��%��#|e1���D'+%?��g�7��9]��4���b�0��ꊰ3U��E%�tK?��C�a��Bْ4�X��k(<<}S�8�4pq�E�,qi@��~������+|�n1�}��^��^r�:�y�D���R�3�'f���xՆW���qL�� �������42A���.�o5���rL�n*����%�4u"�y%۾%�J3M9'V,�4#Zf��u���.�� �Ʉ�~�.z�cK����r�{4���6p�O�Y�2{���>�:@�K�l��-��2Z�?>����|����UC���ӘrY@I?�Z i��V�l�N�	"01C9'��,����sY�Bɔ�b҄��h�E2��&��g�l�C��*9��8$NF�V�fu��o��dtEY�E��a�K(6�S���SeƩTܩLz�Y�y�M��K�1E%�����#�%�����:�?x_BJх�(��dZ��8E!(������t�+��91��ؘ�!��WX�\�踸��q����,3<�%L�$�/"Gu,G\!��BV��p`4+�&qʁ�92/�'���giu���b"ɒ8)"���n��u�du�A�U��kt��XU�$e���Vg�I�2Q >Usl�XaX���Lb	M�I�Jk��Z2�(QH�^6i��En�-�93Y��g/��{�EJA�e2�1�G}=2;IN�$�2mX����4����̭O�ȉ�^N��ȉF"kⱒLЫ�bdTn��c	�3y���#��'l퍘�`��-��?ӴQJ(���Df���)]]��
4^�p<Z��%��^gv=��< �����?�Z�����f.�E��ڂ��紆p����$@¼�iHX��\��GP&�H������/9PHrPǌJ<%3�';��k)Q��"y�����<� �3T��H�Y��`3*���<�[�ŵuh�«Uĸ,+���6O���� aA���F�/��,�N�Ea�ŕt�:�miJ�1� �dC@1Q"E�+�y��
�|dY?V\N9(���r+?��$��kس�6I�Ӏ5B��f��Y���,:]�D�O�dbA�μ��NLF��I�tR?9���_�g�n���;��,�7@2@9ИB� �w�`�����I�t�Ip���o�������?uS|��ۥ`���L��픘�^p�G	zS����Xr;�(�?��I%2�����U��ׁ�'��}a�ԓ˞�꣞�+Z�{<�_��+gQ-�F<�5ñkNoT�Bh*4�#�u�@'UHحл���ϭ�8t�`�;}��vFcp�fs�w�Ho��>ԄٻE��:�_����#�,�ę��|�3�-��x��s�N�f�(��y�e���Mw ��T(��0O�h�a*�@�ǀ�P���C����-�	��q�J����` ��&O����)����!�{:��a��Ɏ�L�ۥ���]�����!Ν�H�GP��d?g���>`J�R�7�(:���Af�f�.�Z,���w�X\��z�O�P�C`��$(%�nA�*�U�
��_��������_��)hs�'4P�I�?���v
?v���
 2�) �gF�A���P"�:0ق��c�(��M=\E΁������u�!׆���ל�;�l���3N�/���2�~���K^!O�r�u�����<y��i��L�ҭ���wa���n��3q�����f��(MY�:�RC:� 1�o���m�-ɻN%qpl@c\��0��r���#n/��,��f_p�,c�C�<RL��pƶ�4P��5�4j�����) S�Ri
^ab{8NAP%Sg��ˮ�2���a6�{�G᫯#�����r�~V/F�% �=�!�P�|�wq.�Cw��E�B�A�ö#�ĂpԱ{h(4�:���{X��S��]t�C�.��Yx!/��+��EH��$����(5�-"0<�@(�����:��YȢ	M�wUu�O]�Y%�?�*�W<ou��������e��{d&��:.A�|;D� jY8>P��0����7�d/��}��`�(�Vs�� �SL�I�����2�9!�7T(/L-�F�\����wZ�i<�4A���hX� ��S���Y)���_p�q�7:X�Ȭ��q�W��t�B-a{��4�G�]74�"k(�/uؑ%%�
%������$Ep�I���Pg�#��3�Bk"�_	+��]j�Fp�c�-$�{�]-m��@`�'��s�cm#�w���T5��j` p���6
��!0W_�-�-���b
W�8f�`�nNԈ�Jfbt?����E��L�Z�k)��譖+F�j	��@��D,�j��Z��Xe�U�Fw�ł[��[��v�KhG�+U����ÿ��C������M�0B��5���ΰ6��ԁ�x��M᷹�p[���>T2�J�������0��D��o7����o�*�ɂ�n����Bi?峘�����)�e�%�)�����\��(z("���p$�WHJg���:c><r�%�q9�pgע�`��2i*:JSA]�M?(��\����oFJ�Se���}�#R$c�9�Ѯ,߆��PP��oH�*/��"ٔ>m�'3����@��[��?�vU��:�+85 p��fᖔ�z�͌E?���O�O�n����J%�v�
���U���.
�̍�
�� 嵃|�� �Q/$�$��Lhi�p���}��Dey�֪)�4=<��]:<�=�K�*_0��Z��M�uS�4�DH�)"h;�l(��'z�T����Q����}�����M��J��
endstream
endobj
31 0 obj <<
/Length 6652      
/Filter /FlateDecode
>>
stream
x��=ے㶕���o��C�'��㲷�q\��Υ*qղ%�4w�R��<3��=�$�[m�d�ʾ4 �圃s��zsU_�۫����ͫ�}eĕ������ݕ�U��ծr�^ݬ�����B��,�������;�\'/K]y�adz��z���R�EO��A�{�7T�^/��_�!e���8�_jQ_k��+L.D�{�$���'�_{.`?���g��`��_�X)t]�z�H�)O�� o�6�����5��ZW�Z]-}l���8�b=��V!�%`Y�!`Y�)K
��9�������'�=A�	zO�{F��Օ����xi��^z�8�_��P���RO�$6��3T������,�w�~!le�!�� ��Z��_-4*�=W�,M��⁋G.X"k��r�m����{.:.V�t	Z\N��[�kW�֞����b�Ŏ�!+ y���5�]�ڈ�K�R�SD�P%�
(f�_Q�J�v�x�x��o���2Ui���i1�$�����6RWƉl=�(O�
o˧>TRM�@��� �
�3�mV)�w�$�R���xQ��q�}��/��.���X�l���d���A���hQ!l����*�l�܊j��9����Or�K7956Ig% �?+Ц�Iǵ��A�,�rB'rCvh++�>UIT>����L\��5S	�{K�+��3gW*�n�+R�����9e�;
< l.>� ƞ:��B�0IRRȚ��e��^O��Qԉ��Qd�����c�l���H���^�Ԝ")0�8��0�����@��≀z1� �f�[�󯎋���2�	!���P�r�UԚ
�4�����3�i�CY0��'`)��Bda!��,$ )�5K����P�I(��<�(�dX��5i�O�t|5
+kA�����o�,-����y��ȼ�S��*�@�m���*'=�i,�)��6<���� �-[.P���X8c��'L5x_��!���UI��9s�#�/�WTA_����/�_�A%�;V#.�w���sF;A��R�E\�X�����N!mU{���Y>p���׊���Ԫ�G�i;Js[W��c8���S-Οz�<؈	?�e����l�ĲA����e&�S��� ��;�@�}+�~�?=��� X��^�lXM��6���m����ĶK�m������}��S0� t%E �Q8����~)����T09��g�?��'��+�ޅ��0)gM�,�r;�_H�7MbA�=]�+g^��J��+&�=*H�8����Rב$���w%�֤��(ImHm�W�<�Oi!���y��WH���@�-����7�j������Wkx�s$��z>\�ʐ������0J���*,�ʥ�`���C�� ;�t��Tߞ9��:�Զ�C5�U_�C5���x`��t	R��ե�TW��JywlHU��Rه �ʚ@h�/љʊ�:��<��+?�$b��eT �����pB2��B%j�� LZ�f�N�R ����~�o�&��B���4�cfժ���v�V�J{-���c����n�m�����>��X|<�y0z�*2�/�?nv�E� �m|X��m��p��y����ݿ����Z�w��;���~�{����h=��k���?��P+G8`��v�y�qٷ���ř�r�惧��n��#����7�����l�w���]Ƿ���b"�M�Bz_�>��S��ثyxܴ�ݚ�>C�����6=[F���[��	0���.�h��=WW������= ȱu�����f�tﶯ�1+���=n�8V��+Ay˙��P0�7�Oꅐxh�C�&��a��v��&���r���%����]���f�?�S��b�]�o���=�n6}۬?��u�_��m��3�b0�m�7��9H�`��n���Pz��9��O��]%�7���Cl�kYG������cPʫ[��G�t�9s�o_��-�e���1�7������M�:j|lz~%����x�m�vM�+�%���Jg!H !b��lD'��
W��?:"��~�q�pqG԰���On�6�s�|&��.,~h��k��&]����JD ��u��Ҏ�v�Mc��[�O-��n�pOW�Бa��Z��d��,H�/�x��E��n�n��'�ր5���nk�����Rג�-�b���&v��� �a�m�%.����>�4oi���8̖`�{ݵ�p��£f)�뉛A3C*�)���o�"�j5%��F@ޚ&���2YD�~���l�׎�~��ݜ6NϾ�_�\4\lG���ӕ��%�/`1U���_��X�S4x�j]4?�jC���\t\<��wk�q��>����5X�2��ݒTP%�����_��b_������o���kji��������\0_+�d��Ԝht�/�%D#�K�&�c�.�Ӹ�yO���=yړ������3��ͥt��`�&�&���"��L���ˍV8�RN�!f�XI��(A�"6PĀ�ֿI��I�����6Ԏr`�t:6�V NΉfH��_C�ҍ������;E���)��,e6a,r�" �H2�H�$ � IK��@���<:�_��@���\�s�H@����L��rb��r۟&'���̑��K���,P�,��9F;䟌:d�������`P���' �=�\ # �{ޔ@@��M�<��O]j�2T��;��
کs/��sd�dr�u�#\�Ts��{׬{>���Wy���4�s���E�N|�I�*-V��x�S]�rζzz��R��fsV}H��2�R ��P�%^ ZɎ��5ա����{"�$�%<cߗW�@��ȓa$Ͻ,��z�ǘ��4L���l�2w��=W�J��RIB�죔�j�P��J�=�ɞ��g�v�'����<������V�іt]d�N���Ap���C�7�s{f�Op`)+��'�=5.�R ��&/S�OU��-�3<]Z�c%Ox�{��Tl�;��3?e@�q�%:�,�uf���|�tG�Y�$*���.)g;��� ��9����S�Ӳ/��:��?k@)�z_��*��Zui>(���
	~&K$�q��(D=�Hծl�` �H�q����Vi����,-;&�ǔ���UC��C�i��q̴2�b��(��8ɏ�^�S4�+o�?�Ȑ˚gH����9�_b�����!WE���7�Y��ʂі�E�j������������n�U� ���1��y\�Dc�>�xZ\���ۛk��`�Z�՝��Z��|y���]Ĝ�+�i�D�{���ݭcƯ��� k
RT\���������0�����[���o9$��Ls�}��,D�hnb�)�H���FO��u��]�[Wy1_0�Ϗ�Wl�P�Q�h�~r/����]��p��,��\�s�9����Q8��zF�$u)�M��1�h�h��!m�s�{�w�aEZ�C����<�qKlC�-�O�O���9�%��.α?������{��@8�8�����"7��2�M�v�a��[��������nc�W�h�Ģ�y�Y+��}7\H�a��=�X�0��eG�o�C�W(Ę����#҆��ـ�'WhJL�4��Y-z����7�!f�p�>x8 �CàBwbg{�d�T���DV�K����~�\x�f�{�&����~x��t\Q"ͶꟐ����=�/��%���ҳ7��M�Y�l�����b�yܠ�;*C�O����[o��ǆ0g�SD�k��� �r;b
{#�v����}��Z퀥�w"��LD���:<����]
&<[�!���`�<SC���c�	b�]s����Rh���M�R�5�m�$x�Ș&肐�!nZ݁�B�`⍨�7���UH%�wȸ��y!Rp�U�vt2����j�Cq�  ;Y�.��c� J^���fu{�?#�SXߌ	���B;'��Q�L7�$���U|��Ì1��-p���3f��v���`�ULA���VA�����A�]1��]�NJ���ڇ�?�l`,�%@U�CK�?��h�mH�@�EY�S�d����i�@燓��8&�a"' ���i�Ѐ3A/�[�9�f��L��`ͱ���{�Ă��"���0�������~`J[Yͦ�'����yj+4Q7ɝi�Zy�r��)�8���.�z���� �1	r˳pZ�. iqސ����<gi����`�s
h	Zv<��in�M	0������2Jd�*kTv<G��30S���7%$T�|;<��H�9�(��;_���R�o�5�g� hO�{���-�1�a�Ƥ7�g�?-*7��~]�UZ-����Gpﶫ�a���N��+;{��n(Q��L�<�&���>ݶ8N��t�ț,"�>�E��v�u�r��=%8j�?����׺�s���}O�P���W!2��v��`����4��o�Q�l7�>5\D��	~����@�:C���a0k-�w;�kQ?���dՔB�޴ *��H��K�`�>�����e-X7�&l�  &k	$�]C�dv�Ǿ�G6�a���w+:!���
s5�tJ��E�m_F�ѩҵ�O"�S<�3(��2E����Ȥ��R�5�e;Y�y�o˔=_�D@�y��I>:�О������(15�ј�K��� l�RA��$�*B͆�o�d�հ�I��n̫v[�ڥ��+N�}V�3$J*�OGO��/(�n~u�H�4���΁��'e�{�Q�1���¨��s��P���o?UEa�D�{�$����p�,�۱d��S�
 �.ԧ'
�>�?�6�(E��g�~{�Lz���C���.<LV�tj42��8�ݷ]�Շ��*n���\�]�}�2W@1�����6�65q�h�0�1�GŤ�F��W����f�����ρ��Pw�*�r�'φ�KX⨅�-�ޘU�o�[�E�C�aĆX�J	�Fc�''�C#��F���&]�
��Zy�����%�G!�.f��k���[q�5��1�eA��)���ZE�ɭ�#(��(���������SN��m9Asۤ��L�9`���T�l�I.��s�|�D�En&���L��Ic��gFY�ut�B�m�yB���;0���z��� ��*���j �<݁��k��љ��!��n_�By

�L6;�!���/Jc�j��Zb�h�����@��vҎ[��P;X��rt��J[�۷ч$�e�Y��a��3�TC��?�;@O�0��f���� �|�	��[��Ӎ�+�PQ@d�/�(yr� M��2�(�6c%�R��IZ�<�7��p`\�+�v��@��n��m�>���EK�e�^�M��ݡ�qW�<~�S��N�˄��U�<�,}�,��B�==�,���Օ��l���F�Ca:V�>�/���;M��t�Y-(�9s�I#��٥���\G�h�v:�$֥v:�:��"��irO�3	Eբ(}�GC7��n�5d ������?���R�U�ٔǰ�0ٸ��R�K6��3�_�B�r���	�
��w�S��|�R?;#
��p�/J}Щ��)���6����KקH2��{��lz��i�KUԭ�@�}V,�0`ij�ͮ/Lf�����"�����ߗ�gX�+�B����,�ߵ|�h���Zu�_ �	ϑ�Ooq��rfv��/��%��(���
}�T��%�q�y�s]�h|_��"��X��2�bt�7>��"�s�f�^̹Ǝ}�� ��L�0���R��{s����[�����"�7�8�E�wȂ{��l��w n?d_�����<\�`�vuQ=��RL���.Kvi��	�>h]sJu��C�ˋ��I���A���d1&`s�uĂa!����qQ�F����.7Ǯov�5�Pwm��z`�I֙�+���TO���&u�`n�̫M��j0�%�_���>D��0b��C�,�ўز�>[���hzL^�����m睴�s��D���Hļ�٪N,�h�Rv�I�fr�Ћےf?�>��eKu�\b#ע�[�kPnf��%�Y	y�.s��i��)/i���?�)���9!����tS�8��0C_{�\����bГ����l#1_�����T\��o )5�̱D��o����������h^��Y�Qϫ5"����k�Q��qeҢ,Μ?�r�HuoL�����6k�D��R���l�8���d�X�Q�����L��$�J&6��uN4C�A����C��8�l�nM�;;n`��<6@ٙ?��?�r�
�[�Q2=�o���~t��WUg�(i���<�ՙ�����'�'wom�s�ɑ�H�s�)��R�{��ҏ߽8s�Jӑo�~��������Ż�3��}Q)�*�l�`s����X��L�/pY�ʟC�U�^���ĝ�ض����
����\��0���ϭ߽t��ˋO��wE�^VA�A���������c%�ܲ���0��KԄ��܇�v["��9i h�<{�@���tg��'�0wDE�̩���g�g����w�
endstream
endobj
37 0 obj <<
/Length 8342      
/Filter /FlateDecode
>>
stream
x��=�r춑��
�ekT�ap�؛J9�8��n%�>U��*�u�x.
g�����@������sv��Aݍ�Fw�ܼ�!7��~��_��)�R���ÍT�!�FYH	5˛�f#\�~����TFM))�Џk�嗿	ml�FF�ͷ�s����sf����\
�����sJ��6��dP� V���k�;��̴�e���%�T�@�(OE�Ӷ�_��z��}	��d��v�z���O��*�Mh�x��Uf`s� FZB�V��:o^0n���(����,���CJw�Z��	!]W�Й��P���eYe��mA�.����O�zQ�`����z���)DW��*�<D_�
�-3q�Q�F�`nda5�q� �':Y�d�����տ#e��Bp�b���@4��P���(�F��G~�L$M�'�`�W@���r}Q�����L��n)l��(;@��3�����`�#����
�̩1����rSpہ��cg��d�c�l7�j�ߍ�) �`�����)��]ĦP�ALƦ����z���x(�e�� �k���.%ԧf�<,��-��o��96���}��/pF���Q�Ұ�����'7Kx���8�k���� �vu���6�P�'�R�#\�
��d����)��0g�[�:�#;԰�i���n !5�w
!*�g��H �5����������~��'�@��t {s�{��>��c 2�Rv�s ��g3���3(N�����f���d? �6%A�Q1,@�
��c���l*�R�0'��a���Q�BS _��lL�o��d�[C�o#9���z��9.;g�l�Ǉ��9 2�!�ae !M�I��v�)���+�$�-1/den����d���g.�� �k�Zq%G��.��;�[05ꕰ,&��Ƈ����"p�u�S�m�r��i��)��n`H��D���te 9u����qW9~�QmR�9hu�v+LN����:Ӂ�@β�@�������y��ԋ_cĠ@G�3�{�����U�'�AE�'�Ãl����50n��]P>)E��4�����Q��!�IZ,���Ѐ�1A�y�62&�/��e'�O�#&@@��[�3]�<��w�'��T�r,�P��Z�L�eҎ�v@��@���_H�S'}�a��B�mW���	y�_�s���� �	�q�P�����)"�@/R���5��
�:�&0��J��"ʧf�;�7r�@��/7��]��Y���&����+�=e]��X� ק����PL!4�x��p$��#<�Ļ�F ��嶑w����:w�+h 1������bZ)�E^��Ef�Hxۻ�����y�Fh�7>����|�����U(�h3F^Fs�'a#b@��������%ѧD���J�`��i(P�l�ˁ���d�`��&��L�O_� ��^��-�'�1�2W[G9��֛���5:���!d(���5���go:{y�p��H�5W�D@$�h&�*�=tE�S�>�]v�Fv
�������SZ�����3�p�g����<�^�gI��rf���_�^��NI<�`���������}Zw��5���\�۬tg�����E����~�S�O6��[̀�YP�@yQ�%tkI������~>4J�YjmIPtX�K0R
>��]�w�����e���13�x���2+�����g�Z��U�A�#2[g�Gc�Ϡ3#�,H��#u���	m�R��	'�i;�>��_�=�-4�&D�}v�2�ӛ��/d��:���38�~M�;n_�D�WC�ٜ�S�
;t"}]֛�����_���8�h o��HA���|@'ŦwX!ō� �0�1VX^sr���Ķ�A���l���-�����>������UJ$�k��J"ƞ>�i�j�^�O�~J���p
hI��e/n�W�3��<��U�KRt�Z?f���?f�)ؑZ(k� �z��
ꄳ�=�P��"N�?�8�M����R3���F�\�qf���޼��ϻ}���툴 Lf�>	��H�:����SaUH8�BGw�Ԭ��À����^6uy��|�v���g���
`u��*��C��P�+ו�G.����_Q��}����g� M�q���x<�E�\,�.�Wr�V5a+��3����ܪ6��?�o����/�W9?z�2��i$�t+�X�(�������.���a�^��۱�γ�]�,���*n�B���_EAG\���.4ڇ_?�*�O�k��Ӫ���1��c��T+��,�����ī y�ko���Zܗ���b�ZU�zQ�`Ή_�f��8�d�l�����r?A��/wa\۠��gu���[�Ƽ�����x|n5�ro��@i��'�"(��	F�~�X�ܽ�6� ǌ��N;�~�t���w��������� ��c�����,l��=����w{N�P�j���{���s��X=v�4s�W���� �-.������`N[�#P��G	`��f���B��͡7+v׎�j��ʦ
����Ƅ�4�f��ɹC�\����(�o�)ϗ^��k���)��I��t����N|��k�����X���q[#s.�%p_o�G?s����Z��=�&�g>��pw���w�r;O��?��Z�<+�xAxS	�p[ |��%(;�whk��t���e<�n^ y)Kŏ�j�z�݀Ts$���Ϻ��%O�V�F��Fƻ�!��u����ê��;"���k?�R2sb <�������b]�/�1�PB���'_B���a�}Έ.tO0�,bY�	�����~��E΂L\�' ~���P�����I鎴/���Y�k*\\n��B!^\����7��o۶ȋiae��\˖ˁ���#�le�X��~���/|@�(��>��i2Pxl�߄�^+��D��uAT�$����x a�^ܣV�YBQ�)a]**�؟��|]�W8�|��6�޶�� ���A�py���W�o����f=�7e��^ԛGm�g�a����.�E-��?��i��D�5ֵ͞�_���W��b�+�Hs�8�X��$�;����+@��U�j��H��RD`�J8��ٷ�0�h���*Bs�h�oP�_���j]��Pa�ן�`��a1���N��sJ�N���z9eȠ�TxA��W�������o�������a�o{
������2���S�sI̾�7�ʃ�e�v�m�v���a�Բ+��E��Q���@��P|m�R��j�sP�y
r�R�v�j��0��wB�E�z����^��07��!��P,�ݗ�!�\�_��]kRI��}җ��>X_�Р���u��X��)�o�>ûTg��aZH�FE�e\H{���x�7Y[Al��C6�--@���-vh/Bs��DיAx�m����ZH3�>UM��69�]���P|pS�+uw��!.d�8���+�U���1Et���+ʚ�N[L����<�=1��_4�P��/�Flxu�E���Up�� �V>=�jg�j$	��0�Uz	+K]�1�	�3=����i��T����dq��b/�bO���R�p�wkȬ��j��9���eƜD��I=w���i�Qz�A��[����tcbߋ�Ne3ӚB�f�2�
 �+�jUB���9T�ԔrG�Ɂt6$�$dMXRzR�lq��7B�4��dX��|��8��Rz��^ǵ���T�s����`߬V+_�������O�[.���gB�֑�Z17���$�ډ�f�]?mw�1~������݂Ɛ�U�'�����������3~OIN�N��%�����^�z�
��s��1^^A�J�P�:��+�۱1�htXB��n֮�c����g	��u���-�/����B�5t����zŭ\?9��ó��~�Ad�{<��tq�FgV�S4�T�|�9���@=�͞?:'5������̀��6%�y���d��`��z����@T!�y�FE��z��$]k|�T+�����=��vQ��_T��u8;��?��'�	���k��z-pwz���z+v����˩aq�1:� �0oC�Z�N�����n�Y�t�!=h�m+���N�o!���;��3_�NSCC�S���14ZV�&��m�`���\0�v}��M��8׳U�y���\���b�Y���������Go����T|(=�ϣz�S����a�$(��=�N6�N`�+�X�y�����DQ%���?�N���K�ߙ�����+��0<*I�=��'èŐ��v<X��Cm�Ь�f�)o�)�����G�>�x�c�Ñq$q&���F�>2��wX?�)���Iy#Pv���T�ԓ�]>"����'�jY+

:Z:�e��(ҳ���8l�N��%�$~8��D��X6CK�t��| ��&��'e�s���?�`��F'�y:�R�A�4g��Om:�rm_���}baeW,��4j�œ.���ⅾ>S�kc����"�:k�¥��V�blc�\:%�$�l�"��W�fW.�� 	^����_5�6���!eM�>�g̛y"�-0��`z��,XA���v]���o@�#Y]�P2��y	�v�ĴI���m$���˗CP�t a�f*������ ���^��t�J��kC)���M��Ʉ�}_�f��8^f��%��r�]���'O�
��}�-�Mp�7w���9#��`4�����$�k���� ��8N]	���r.cx�KJa�jG�S;��2��K4�0�3��n����h"�v����s�
-���dRRQ.��s��s}��)��W�A~$��E$=z]�7��
��tn1`*�д���B���\eW�ѹ8�Dt��O���8/��!u�aq�Df�M_`��T��/ ���`:;��uT��A��`�������kX�$��8P>=Eg�?;_ċ��tF�+4C��Ɲ�,�~��`��uk1����)d�k�`9��drF�+�_t��HEJ�,���y�J�{��|Z6�֡�N�����	Ș��k�2��Z�q�c^5���������N9p �m�:֫{�T�o�a�j��V9g�!l?3t��t)`���F�=b�����P��u-�#�p�}�>.ܨ��s�BoD��q�8*r_d�ۨ��>�0�Døo���)=�ϲ�Z��SG,�3�]��B������VË~z/ڧ�Vș���*�1 IoprlO��>�;����xISH�:6v���9w���վ�ei^�,�Ŏo�Y,�:�Ә���-S/��_�v}z$l��c�5�+؀'~/0����>:8��x��s��1L��'�3>�iͩ�<��)�iM�-~'<��T������!HMv~�T��b�|m����=�\�׈V��l��3�������0�͉�1��2�sX��cX��3y��=n.�ݢ���-t���`ہg��Y����f_���·����?��K|o�(^X>H�RI��oˡ�܄�[|8�l-<daI�0<��CT��}������:��}°����m
�=Y��sF�]@g���Ij��͖��0���A��U=4�u6�Ǹd闐[�{좱{�T�M�``%���� B �hg�u�{�{Ua"�|�[�M[ ��a��,�zfĈ>X����*C<(��M�[Jz^�+��]J�Ln�&�FB�&>�X���^ly�ߥ1��.�V��o�F��������r ?i"�)����G�E�fW/t���{԰66b>P3�eߪ��87�����
�z7o�0V���q�"�u���AE��]~�T�3�_�3��t,J��lGz��*�L�&��U�۬���.�=�� ˇ
S�M[4���cq�6�w���fm@����v�ƧY�<G�ͷ\}���w�K~.T���T"
�_䴽m6�����ʔt��&e�N��b��8c�q��/n{_��%JyC�մ<�����d{X�� G����P6^8`zV>�Q���æ�4P��FSt���]F�ƨX�XNQ�xqx�j�Y��0�H�-0��$��a�8�@�8���jc܃_�4�3�X0��}zX<��uoL�I �4sF(�=3H�d��,���9�y=�]���.p�L�l!S�����I��$C�DχZ�y�t�G&rpV�ǀ<��GL�T:\uD�C��D29D�� _���<�g��/��������$�Ɣ̧�X�?�	�׏6�.���Թh��ĝ~R��
(�7�5@WU��ҿ�Pضl�k`ͳ|���j��0��W�3.�~��<��}&JF� '�c��>���k��TA���0�:����:�uқ�7�#
)f�.��Ӷ	�z�h������d���ΐA��F�VP�q���[|
iL������L��
��b�y���iU.�@y��S�M�1�u�J*ȃ��J�)aðZA��XX�2�[H�O�G��8E��a4<3emo����p�.=�14H��5�(��t�KB�;t������@+���@[����Ʃ��e�r���?a�}�#��c�i��;:�#�L2r����#��L��=�Zkb��E�r/G�@{��S��
"�/zpY�ԉ�bh$�+��[�O��q�`�9d�]MY�Maz��K����&is�e|����m�����YɌꛣ����o���y��e���UQ���@0qjQH����
�!����AMȦ�T�.Ǟ���\�!����
�'{���Ð�����+<����a��%Dq�6#�2x2`�u[���*����8�v8�~ჹ7�_w|��gk�����.��FΪ9��Chz�}��y=\}�W���]��q2�<��y�f:� �m$9�޶)�����禄R|�D�C
��	ې	2�}CE�L��aZIĂ��*{���c=yv!�����\�� M�y_φ��Xc��m�	祅�ӺFN&��ԓ��=��ch�3O[�����!E���9���7�������Q��o��>�7i��>o��Χ��ϧp?61��~�WZ�%Z<��=�CU�s�<�<,���1�x%6S�v!�釦�W� i|�G\atz�Dl��L
���o�G��<��e��ms�NňiE����z����3��r����7R��0�-f�ms�R��7��v�*��Fd�ܦ"k�Vu4�T8ꁚ���ɵ�/���}�_�1i�^}c� #I�����\���a�[ܘ��;	Y����cGrǧy[��mn�zͻ�_���iE�-��9e~ �q*��e롧>��1�%Eo��>�c|�O��� x��cn7�F{
��`\K^�[�u�F����~z>�E~tƑ�;=8fҘ��Vt���6�w��3�p a��^���b��$�3���B�]bLt����d��<��*7��!�#r���9��{����9�s�OΛj`Խ(I�wܶi��c�;g8�^���q��l�]����T�����S��v����wr~ z��g\�fdz�N����Elwe!�����zG���ڗJ/��7,5���'}sR�s���T��Z,=z�!���������w��ƧU����]N���I��Ss��2�!���6l �?���̆�H�H���y�DwLg�^�vo���if~��&�3V������{L�*^"x������BC�vQ��|�mK�T��>{G?���6�(9����u�z���$o_?|���y�����ηif����[w�p�Wme�PDa���cG�r��|:s�«u�i
��ܺÝѣ��8�%9�\o���C�������<Jy�	/P�z{�"I���E�K>4��{�ۮ�����{
�p��meW��2x�bU���.�U��A1 v�2����'<��S���[��� +��c}_��M{�&VE�Nz�>z�� '=���k8R��r�܅,8�4?����:�ڧ7�z�W�Rv�濽�Eo�������O޹߷���U37� �=ۗx������oŪ�O�>nC��o58��WާWu71�,��'~�%Ɠ�r���{5��TΞ� �~_�:O��(�~�p1�x!]~W<lisCu����������Q_|���5x6��ʟِS5K/��_���\λ`��w-(YOS��_�#R{G��gB��7��i��m]9����8�I�Â�Rw��qa�p|�Ʉ}C��l�	��P�k��A���}�#��U
endstream
endobj
41 0 obj <<
/Length 8046      
/Filter /FlateDecode
>>
stream
x��=ko�8���Wx������Mu�1�����b�����`�XI���Y?&��믪HI�Lٲ���,�E�Q�*V��|r7�'x��~��z��w<�ج�ZN�n'��d�&&gYa��j>�8�|�/~���W�)e2+
�P��u%�	Jp���L�+�������eT��d�CU�b��d���R�1躰MF��G���۫7�x���&L�,/8�If����7�'sx�G�T�b�DE&Rd�pH.&?��oq�1ϴ�����&�U��Ȕ'�3jpo��MI�>�<��m��R�#��PI Ta�e��;6�3���1���j�c���?�����[@ڶ����Tu8�d��!&S�z�!Q�Ϙ1}��� �;��@�9L�L�=@?��6�e�q&d���T�'3��d�1��f��w�|�d��d�ND�^��Y�"�׉V�Ѽe?�	"50�@�c�����q`
ں�Ƶ�H �gFET�L�)!U�M�i��)���eJ
�ۺKTU 6xV�����
�ra�p��0&���q�BH*�!�@3�
ÌyZ��h��� cu�E�[,��@q��D���LVA��{>v��7X��B�� nhs?�<1�B�U��1�诠W0D���!��&�� �75I�I��_r��F3nًNcވPG�Q�ϸ����}�F�ʡ��+;
�f��f��l��y6G�ٌ"W��|�\���p��J�̎"V���9�TIH6�\_PX<�&���x�XYD�<-�j�'Leְ_HL=15�)t����2��0z"2t(b�CAyH<� <-b��$�XH��K6
*��i0�FBEɉ���ȫ8޼˂���c-@��澫�v�f��Ō�|*2�I � �-LG3�?���bQ�]���\n�m�Z��͗Ͷz�14�ɬ`0>@&�ㅢzQ��헋�0vz�Z����޸����M޼������r����u5�ݸ �L�+����7.��^�T��
z4�V�ᗋ��z]B߹$���\|�u�K��_>��g��n^�йh��o?r_�������=����ru���i�iܘ����D�u���wP&/�7�嶺`j�y;�]W8_,o�Vm.]�����޽��ۍφ޻ܧz{��V���ګ9艟.h`����ˣǖ�u�Pш.����a��$�C����7����R�����{x��/tsS�+����g߮U6?K�����,�3��$�ټ��>19s+hf�Dݪ/������4g�>�����#,���!�j��r9Op{XU��}�8^����Za��L�d\-��HKO���r �޹ ���?c�r������3� ˭�oq��E�:f2�[�,��U���0��]��Z1�[q�@��X �AcnƘU��W�v�z���ܯ�ʥ�~�ɠYQ��%�?Q�gƃ'F�3c�͌H����:�F�}�Y����6�XM����B�e�M/=^p�޺�%0��d������\CeJM�+' 
fZG�J��Hlq����tًj���.�d�!1~�¬j�4��;��
��cQ�=�L� pI��x�ں/��fg[=�7�\û��i��<F=�CA[���w��ΊL�~c=�S��P�( \��$f"dB��L��i��4�`4З�1�R{��4(��L�5����}���aS��
D��&4�0G�<O�W�}�ѯɞ����>]���.qԭ��f چ�I���Ķ��e�w�>��T[���u�ȭ@&f��Wz��K�W��6)d)l&;̻J.�M��c�m�E��!in@iV$�0/��nj��	�խ�<B��Ǽԓ��_���b�k��s@�NX�5���������r$�hL<i�b]-JzJ� ����|�6�'��W��9�P�^"PX!H�_�6+�t�[��S^�t��`U.�����0�&==��P�nj�m|+n����?9����zQ��/�����C�L.R��xC�4��9�4�4��7���}�u��ܗk��j]�v�~�	2�l�%��2�{����@'�F��#���oSN]�������4@���5L��=�^3��GFʙ�n�F�:R�Q2�W����ᛌ�/ߗ���iǗ
�z�H�w�~6�?v��JQ&J����F��$R�x`�v33�^-_\��u�k���'v�tτ�X�dV��3�1`��(�*^a z�SR��@�R�=
|�u0)�N̚yi�P�6ݸ4M�b��K��4'�jȯ��f�,��q��?�rr:���B�%��s�py������ܑ$*��EՉ�	UY�]'�y�V\�]iH�h�$kR�Pp]ݔȀ�R�I�'^Z4g�͡�e]Fz1:�&|x,�02g^�N˥K��Tu&�1e$����
ɶ��w�v[ꣵ��0���Jp^^d:��(���a.��t�*#����aS��=) it��5ưEu��-au�*� �.zf�� 3��Ym��`����Z�^���6� |��|��8�pJ-��������L+z�g�Cl+�id���r^��.{Q�ng��l]��o]�H��B(Q㛮�`��7�G���F O>ޮ�l>�[�W�Ek�"�ݿO1�<���<���=j,	�Q$2�Ϙ��W�3�l���.�B�L��>���j��w ��������i�O�9Qh3$�]�
k���  -O����8|�UR�uѴBW�t�a�E���Ԯ
�E�����Ơ͌�]���f�JР�.Z	��A�t�����Lz]W�v�UѨ�!��@�)���EڪN릧H[���iZ��w^���f��mn�͓����R�CyW����|������.-�l"���HGϝa���t)$�e\m��%Z���1`��t��Ya{��)ˏȄP'��z;���6���
�k +��wɕ�m-F���B�n��(G�M>@�� 
��G��׆�8Bm�o�����2j��)�����V�.'�s�ߴ�',@��+�;�� pF�h��`B�H��N���s�#@Ƚ��h��μ���Č!�� Uд��I|�3☵�=��Z|��Բ�]Ag��q��&����	X���8�3.����y�,�(����g��K�\m���&.�a^�De��k�Յͧ�\�����a�X)�4K<�	l�������@���M���3���Ӕ-�4_�ެ]n�_�K2w"��ى#�g�D;�M�cY(H��<n��)S�$����G�J��5��7ր�KW��}��:S@���싁�H#Q���kۭ�WI�3�7^���6���$���,RX	Đ��{P��Az�[h{��$b��b�������Lآ�Cف�	tbWG�i���c�P�!����$@a3������u�q���DOg��8)
��YW��&ff���k_Âs2�AS�w�ii�cz-�W�%7��\��������1�>����O�X�q� ��zףu� c�y%���u��X��^Ȇw�7ե[E�RecSf�51�"ϔT�8d rPmuVe�E�}���24=?I��ُ�� 1 ��꥽�i;C v�<���ܳpF��!g�[��#͆xfͳ������ي��GmE�Ź�����#�'S��o��d	�n�~7����u��r`<�.�E��>A�m��I���s�x��^��-=�d�vА��n�E�C�Pݼs��� ��S�I�T�$��x_�c��S��E��ޯs�PS�,�uIO�B�]G�*�㾄w��sp���h�w�,~Uf�[����:Aw�!Ov�=#{�͸��IE�*�{b*��i��윴f�!F@��p #��C�%�L�����{���K�9�z��yg&&�N�D�ܴ��҄����������n��m�+���������j1�=�����Y�R><.�M沨���I�>���gy/D{-(A6�'�LQ�$gIk�ߩgɍ����C��?udD��T�ɍY��eƦ~'�!e^�S<�k��<��;%�.�*:��A/E���s�́��A1.z[}+\/�ڲ�,�|he�����<��iq�~�~x�A��t�Ͻ�}I����wI���G�I�xU�jg�����A�~�&ptO��;�q�f`a),���,����M�0����,:l9ۭ�й���s	E��)b'��c��G��vkd�����뒶��EȜ3��_��Z�v�+�x��ǲ^�|�s��M�2��rO�aОs]�=�Xd���J��QQ,f�:
i6���/�����C�vA����}�)0���aҗ��֮&ֺ�bZ�O��p�T�ހ^~���+ǔ�@1q����7��=(��|S����0F����Z�t��Z���t����͏�ď��L�6�CB�{aH�1T��B�z��R��.1@�e�I�c4ޓ:��`�P|Ozx ��ƙ]�����JjO��)�@X8�x^��O��r���ҧPρʝ��2w�Tn�E�����m��>x��3�ś���W�Q���`1�B�Q @����%T<��84|p�!}J.:��-��Nѽ3u�Ep�(9`&ZC��Z��.9���9nKA�Θ�>�A��:?�И����<�qŭ2/±G�k��%�5�.�T���H��]�����ԙ��dȽ�ν3e���U���� �B��� �%+�|^{��YbX�]y�)��2fYb
�-�nU�V�asE 韹��';��[�(sK>`n�h{}���U�~��!u�� �Q��u���>�x�[;�;�sha�aIIq��=�d�F�@�AP��}�N�g�=�]��������,���hM��4�,�ϭlQ��b�r�#Ȣy&�9t�[�:���C}��Hn��A�$�'flB����L'I��Gq$!�#�W�j�>BD�oS�\�y��@L���Α�=�+���<?.�o�YDA[����CXu�9$h�SD�#�j��&4���&ƜNEMT�k�(��R0n�/�؄�h�M��ђ7�o��IM:����k�kr��.XV�v^�c��{Iy|�TX�rğX����Ŷږ�ɀ�!�Al��.C�a�ޝ���{AS��?u�`o		g��.2��w'���9��|��H
�4�`nڥY�An��He��뚬x_�;g�{\�־�c`P�ǡÓQ>���~>���k5��c��kj�^.�ӏ>�F�Z=Tm�.:��E�_<mc���5�
�Fa�8hJ�5��`��?�f�W.$��0
�oV�e}S��5�J��wf�[M��)J:f|������b�F�!e���"��^��^������X�$1N� �ްԙalĆ%ZI�3N��ƩC�v�5�I�w�2e
�aM32��Q3w�p��&�\9e߹ ,�C�q��-Hl���f�
�|�<l���=F��p;��cqĩQ�jg��EVS�:�Nߘ���A�ݫ�\i:*7r�gZ:���y�����♬ɾ
$��U�/2F���'��<ǂ=c9"/wHܸ���a����Շ�o��=���匛9N_��t�3әw�j�C<CN�CSeǅJ92Sz�L�_.��y1���Đ�L��;&�a�O����j�Z��I<"�D���ے��A���������"z��r[���y��|���>�7~�y9�1�C��|��Qd�B�[
;��@��|�V=�כ�5���E/hI#�p�'��c�� �o\�z>��>S�?�bzZ�Q��1��	ߕ�ps��)]�"�ޡ�U�'�wk̢�z,����_�����|�4y�\����7�s�����h&K�� �di��$�nh(l{CC��$ ٸ��*
	5�L1s��/���!Q���.U���Ƀp�LD�▮��]��~��\�	��F�s���V2�=:���~��� ޵�ˠ�oy&M_%4}5��Ú~�:�z��l �mGd�B��T�3ҳ��B��:�$`��Cj�jP|͊�eTG.��v?���֨u<觔�m&MX&/)bsV.����C:$gE��� ������t�	��<�˃��cN�4��Hx>hHkc�:6=��(�?%�Qձ��\E����A�2�r���r$܎ui����D޻�*�-(����V|H*�9� �
CΓ"^�u֘uOqU|��f�wMƳt��{���A�g�������w���'D��ȓ�jh�ڟЅ�=�
�v����CMw,a�@;��|�lO-�:Nw:Hr(����D��1`©wPc��7�9SO������P���>���Gg0��A$�2��H�S��ۜ�k���h��$�0���F�������9�����-y�(��j���nC?��!�9��લ�$W�,3ZZa�J�� ��2�e��)��(�B/�Q��4�4w�*$$J���Ƀh����{�r
���x��sP/���k�h���*@�CJ��������ژ�v�1E���%�����D�N@��Э[�����zMQ��'|Ux'��$J�W�F�a��[�d���t���=e�W�Qi�p�2"�"߻�0�_!V��{8L:$�ґ�B��JF6���jS��b�q!>�t���N�@Z�<@�h}��f�a�s���ue��<4��6�[ro�I��� ��|(e�F��a�aɸ�ܘ�5��A�!g�p�3މ�K������_��A��ak�[��i�k��'�����=6š�J�p���h@qw%�n����'��迠�_]9m��#v��7;���U��]g�ϋrҤ����nM�˘��vM��Ɍ?Ie] z�^���x�_�׽�������6�����א;�z�+�sw�
�;UֽL���҃wukn�k�f�骉��j�n�
���|~\_��W�F�5�#�^*}�����R��;����u���+USbhA��q���M�M���j��4���)��R��u	�P�08 ���H�9�<�mZ���}?p��BU&�Fʃ�a����T�*�6���-;������O2I��im���~�a���������:���j]�t�R"�}��(���ah	�z��I��v�@n����[�<C���fաco����~C��kpL�o!M��j�.(82M��[�gazp�u\[�[
�5�@P'�U1�9�wY�swʤ����t�� �.�����X[�tPxj�LN�MH|`�2�4p��"tߤ����_o!��d�O�ټ3�5��&�m���H�#�i��Qx��ZEq�:hV>�Y��LZ�]S!���ql\Z&�P������u�4T]�%m A�HVep�e/�O���.WΝ����՜��vI��Nv�x��V�����B��ؿMʳ}�_'Ά�L�������)"�O��d��nԟ�*�v���<)p�2~̹<��01����O�J��0�]r�����˧&F���J�Pv��v�����u1b��/o�^w��C�1y&�Y�����M�1��1�@������p�Z��T�V��BQR^!Y6��sRu�`���z�����p�� <���)3e�$(4]!<S�P�a9�;�S�"�#�'�%OGS[��ta�1w���$�6/ܝn�}u�X�V��vDw>9���U��������Z�4T̻5,u�\�ք�����cP�����bƜ[��̍����Q����h�e��4Cp�4�1ͺ�&����4l:����ϫy��Jd�s���G]Sݝ_\VeQG�4gN�n@�XW���9�ֱ�cg�=>��ܬ}9:ug]X[|ժ!���Ǌ/>�</���fl9yE��d:V���j���� ��	����mm�Wh��.��/��i*��8�G�ˏ.BV֨�n1NЬr���]��$<� @��_:��`�Et��}(8�����Q��$$�_c|�A5,�ܛEuK�]AU�:ϙ�J|�TT��Wo�v�93
endstream
endobj
45 0 obj <<
/Length 7077      
/Filter /FlateDecode
>>
stream
x��=k�7���+�����:|?��N69d����� k`53m��i���s���Hv7�b?�����y �Sl�XoV����L��i�������*%&o�M��h�
�'on&�M�\���Z���Ōi6�+.޾�K�4��La��'�������盛��_|��˪'���u��b�_��dԐ1�"�j��_J�f�Y�s�;[n}�sXί����n3_Ͱ�ɌJ]���J2�ƶ�0���vW�xa�onK[h޲�M��_�isc�/�_�鲜ow������5YX��]h�,(g��k6�LJA���`��j:�SB�km���0~�HA��3/���=,V����ɻ�r��`r��~��/�mz���	RpM2Z0e|�_�����+X�$�fb�P 
m������ �
m�dF
k��f?E%�D/�>GU!x�l�>�F�ŏo^�����P&��Di ��\߽��-������/����ͻ�  ��[N^��/O�i�9�&�0� J����@���A��M1�z*H���9��	��1���t���I7�-@�� �HP�ܲ�K��c�O~�{�d��  ���dF�%���o3��B�uk6l�5pX�����M�2���Z lY��<�ŀ�ac&6�11Y�{�X
,��'C�0��<"�L�|����r}��$/	�#���y %D!�� &��x/'�@v ���*a�1^P�B �<@��`�<H�L�����ρ&� "�h5��o�r(�RPf�M�hLU�cjk�y���Un�!��lm�A�PT@�f:+"n��йj��L�Ǌ5 )����"�rH���X�E=��г�R	��YT-p%BW�2�F�3���X�σ�)Z��6��T���F 6�R[}�N1�C���@���	����;ǳQ_iI�e/nkT�M�Mhj�4�cL}SD��G}^��sf���Ų�JlsQ-�X�����ssV�%�XYY$�*�0�*`�N���e)�f���G�f�<>V�C�+�&�ƀe��̂^ �O�ܖ��5���b&��^d�v]d��A$�M��C�}�!�V���=��F)Z���b��[,�����}�B��My���_-K����GG���tz��?�x_�d�Aq{��N�o.��:7o�4L]���R��NL�ћC��3b�)o���4r
Ml����KV��_������\]�����U2T|�~珍;.�7��;:ď��$|����U�\�p�>E�@#�oW��0��qܿ�ƻ��Ο��~(�� ���q����:�8�Z������j�e����Nu4.x����{�=���������.���^ã�n[.��
�q����{�K7�H�i���&�������p�r�����������u�x��i9}�_]�ˀ��g�Р��P(�)F} �A]`A���24D�'�7�9��i+cJ�e k�v^�m���|�w5r\���~������/��m��-Z��6���~.��rlU��7ޔA((Ψi���"���T#�9x���2TjAӑ	T7�6�G��51��S�[NA+�j�ۜ'��j�OEҥ|"������ֽ�R#d��D�r@;��[��`�EoU����p~q���j!g9L K�r����I�u8،r�l�V�dsPn���X|im���ϴ7�`��R�*ڝ#]�{b�,�1~�uz+m& 5b����i�>�*����i��=����_�`��a�Q"��쨬;Q9'��&���Ъ�� dA�nb��<Z9+�(��0�BtBI�s�b+U�im����:��R�n�=�Xra��rs�X��v�ex謁��e����*A�GLj9l�we�q8�4�=��7��+�M��7�RT��+�n0ͫÊ�N�Q�B��$є��j9<�����^�66..."=�59߅���k��j�~h��[����Y��.5Ws7B�H�Fn�͎�Ij����7ա�8���[�kywp�Ҁ�n3�� ��#��v�(Un�0]+�1�4�h��ڐ�Y��q#G��h�������#Fi<y�x��:h�������1Xk4���z� B(�yŊ��Kp;u�$�
��%��3�9�t��̫_t�cj��t56@1>�ý����1�}X�;^��/�ċ�Ld�f
� i<#)�"�:l���ͭHޙ	�jC�AW��f�V�j�2�1V����
�eNz�G�yc^��L��t�44V\Lc9����aQ.o�;�֌�D|V*���?���<�m�%���qz�����rwn�J}	H���p����tʢS9<���C��^�0	ꃥI{	�-pXӠV��֫,�6�ϴ �f� }���(T�u�e���@t����b���K^�E�ɪ�I�7-��E������厗���hz(�2��	)Q�|��J��&3�	�K60c>�H���2���Z�SU�o��ĕ���Q���)j�$VԸb���jlv} $�}�����������Ǻ{��n���Z�^��n`�LO_m�&��^Ly� o�G���$���w�z��W�M��_-�ؗ9O�k)�ڂZ�%W۲�翟<��GF*=@+OL}̬����-¢ٶZ눵?/nH���73\3z���:�gq�cR�4(�|r��x�r��ѽq1��l_[�3��B�������O+�U(
~���ʪ߲�j�(s�,�X�)��ڇ(v��^cL��~Z#�.�Dw�dMa��<Y�[da���ˌ� ҡ1��s�融��F��~���)o���'4<s6�{�B#�!�ݔA_��٥�j���)G8��BGeյ�Y 1��]JM2K��F�Ft�!��R�Q��.��0�߳mY�؏&��(=h�C�)�5��J!L���+��ĆN�.�X��j����m՛ԙDAl�v���[���/���LS���T�C˄P5���5�8��1�*��������΃�]ov΅�U���r{_^����0g�,6���/�bCA��Ɣ����`���Q!��G�\�E��9Dx�]E�0��8��:�A�
0߮�pv-x�;�u��$\�r�J �͘	G2��¿�j�}?�'��}��p��q����_ίg��j�I�t�"4pi1�K��薶KǐpVqnM�H)��!�����(��iE��s�UѲ�O\Nh���Ū#����L��i\����Q��p��huExa�
����zӥw�-�V!�Ǭ�ec�L��ˇ�j
Fc!�������n�h�dX�Z��j�9A/�6�1B�L��9L� Si�2%b*�Q� �?��#PA�Q���<.�3��(@��-���@��P=�*Z��֠��c�N�B��Հ�{���>����,����*kQ)�9uio��Y��ʺ܈5��v�5ȥ���z�1qL[��:!� ���y�^��C~�s�2=���(�1M�1��y�ժP��/�9�j�V���M��
$���s[n�����|���z`�޶"�(�r"����=3��t���@֊=qL�팩�y����� ,S�z:T��!�)'�ZB�>C�)�2$��j�$�b���ی���#Rk ,jH��E���t��̱S�,Ɔg�����1e��hAeF�a74�˺��uS�s�e�3mf䴉�&�4m��0P�;k�³{�lk�j��c4q���
��k�㎈I�zۊ.��_{�־�I�])�c�Ƀ��WY�c�)���".02�O��U�{�Ԫ�O1�M����1���Zq�-銻=C��SA?��	6�r�[V|���vJ��X�CO�6��/��/�囶�*�<"�����v��y��kk��9���c~0��Y�n��2]p���Z�t�sb� u�� 9f���3Ʌ��z��0���Q��`E��rHy8���<ȧ�R��E���R�RNeAk���9��������A6�G��ޏC}p���⏀M���a�6�6�L ~�u��]���QX�
\y�O�@$�A�A��tI4g���Y<°�Է�9�Q��$��UN̟1TI���B��M�\\������C0�}wnֹ�jFDl"�۱���#�&NM�>W}w}?[�L�s����|�>��+��ӡI��_���*��W�g8aX�v�x]���˻d�hh��.gA*'I6���߇7.��7>��#���UF��a����J<��(Z\vh4bp4�[�8x,�麋'���ҭ�
=�J���������'fc�Q@��Mx�\\vM����
<��?0�6�y�Ijy�U�nYe���t8kargzɶ7���L3+�/ s��0ܳ�y��]~���Bܞ�8� p�`����������_�	��@�(��Tu��C2��>۸���7���IF�L����C�M�ꮳ�Ϛ�h�R$t��Q�4�B�(�Q�ьޕ� sj�z������K�&��V.�`!N
���+���0������O���7��qư��\�I�-��UP� ��둬��ֱf�h2���O�m�)/�?�����s������T��8���ٰ"�hU!���>�e�0�3���Ӵ��:49��N�B���t;����墏}�%�(G���鄎�+�؛�U�u��>=|x|��$��Ě�7a��?q54��H�{��ʴ<+8Ou�.��.��9�-$a��Gðs݀�D��~�MX�:�|�PY��4�ǜ�x�	`�j��]�T�`{;�+�ʸ�=���1�z3�%,��LIϗ�_`:���Ä#kj���� ڽ�+m	�F�@��i+�՞������$�~�%=��@Rh��94f%zkc�S:ݔw>� /\�eQ���~/*N�_�n��"|"
\����U]h���w~�n�z� �â.^�#.'�'aQ(�c��v���ܽ߬Qw��vI�KX0ưTw�>�H����n�I���mt��$�G3�c�@^��IaT�'�5�*�Φ2.��VFTF�8�L�-QȦ����d�B�'Z���GEQ�s��4�9];Y&)֘h��f>�׀���aD�`Ac'�b?�n�uu�LM�����z.H��������Jt�����m+:A�B:����ݪK �We1�~�g}��8�Vٲ��
GXl�y3 k8�}A�IՇ��O)k�J��aE�5t���,����4� nJ�f����3�E�>�`#g��?���4�x�n�h�-�/�]~rܭ����|�@�aD�=%���Y�]��Ҝ�T>Η�'��OI;�[�)��z[��U��^~DY�N`�Y�+'	�0K��%��m��Eŗ/sk��胂�I��e�A�үyO�h��T�v����K�M^y�����~��_��cPwpw�V4�ōi�<,!��x�͎Oüuv�����F�T�)䜏�Y�=�*�t�9ҭ�Ư�kC����͞-z�R��������>��� I.E�#?� ����"��bh6��H�k���`�+�ح�j�=v�����U�@��n2��C��?j�p�~� �:-r�(�S9
?y�����qr0�t��-���;�.`
�龳�ѽ'G��ʮ�5�r��`Gx5�Z�:2���4ݙ(R������	΄hI4�D�L����c�7�Od6"G�]�4�H��D}-�c)-	]?��`5-�P��8�8pЪPv�o`����H�#�����*b�� �C�}8C�?k��:�A�r��f�����:Lhc��<.�"K7>�WY�t�p黓Ϗ`���:K�eԄFR���ruS���%_ry�=҆���ڴr�<Ĥy�2_ܥ�'w \�]�#�L�O�s�WS5QUnAh"����=��7~�l�)�q�h���m� ,�G����U_�)3��F:��&T�u�TR��C���p�*��n_�t�/k�`t��}q��S��|߀���3&A������-m�w��:�Uu�&����@�a	M������[�]��[�CťLV�qs��8*����8�g�y�t�c��C�ٿ��0�E��N2����m0��5��LA��O���ْo@'�;y�ѧ��HeC`t-N+0��)�}��Ýx"H�c�nS��s�`�G3�GFPv��GP&PIx�� ���� 5k���H�=�1���"F��������	-c7�G��\�TV*���Z($-�V~��⣗M�A��� �#4̉���ϴ�ԉ=Qi�(R�+���;3���o\�b	��/M�h�X�-k)��鬟{]t��V�k�OH�2�U?:���g��G�]R��Y�z�ܱewC�C�R��`N�G��y&Y�_,4�e]�q�)��"]��Zm8Y��hC1�m�5�(Hգj`�����}��~ $bı�#7�3MP�9�g欫��� �&�F�@��~֢U�D#�%\���Zp`n�t���
�	��TFY|x�#�ϊ��6�yj�Z�q�&Jzr����(�ҋL���S��n�r�$8�ާ[�w���Q�����~/<\�W�7�4����񬋆��B��T���qO����+�f���@G�b,bN����9�v�Ι����}b�#����
B�%�<"H��1�a�=�6ݶ�r���Ar�%��5�}������
���S��eI�w߇���
��2�AT�Y��	n���*B��������>xxk��ʸ�Qx�_�!�k��)� .�>e��N��p��Vz"i�'~�J�H×�h�	��i΋�����ʺTB���Y�C�b��#�-���F=#��p�������9e�]�^�!"�cGC���:���n�:��w��:�2�:�QO�0�$M�ē�D44�41f�e�׫�Q����'�(kAŝ��|��Qӻwo<��9L�-!�1UQQ�VH�B��x}�$_e#G�6m��.=ot}������ɱ���7?4h~���a*��"��D��y�:����k�5�I��/J��
endstream
endobj
50 0 obj <<
/Length 8353      
/Filter /FlateDecode
>>
stream
x��]Ys#Gr~�_��&[u�oH��������{��@��c��̪>����3�/D�Ѩ�#+�˳�ջ+r��/H��������ɬR����+%3+�&6cR_����u��&����_�����QF3i���s���"�|�->���/����=rE��ʴdW���{5{|��o�����ɸ�Wܓ�W�e�p�\\���|��ΔM��^]2W0(���4<vuK2k�h^<$��d��~�eb�2�Ԗ��9�*�)�|U8E<�*zջDK�fJp�!˧�\���[C�[�:}K3���[����Vd�K��B�5�t��(�ͨ����ä�0��[.�䧥��>�7�jV	g���в���ڨ�w�FN^���)���SU���H���0�4#j"���j�X��$,���L^���1����3L���F��QS6��x@E�g, I��dW�~�tNѐj��Q���l`�Xp�W�_���ê��kb@63�&����Kӻ�{;��Į�I���*�k�	��;�EI���<�0k�����j�^�Ux��ej$�DD���`$-�Xh�����ǰ�1d���c�@�$I�;��B��d���D�oO��G%�͕�����S#�e E,�i�?��i��q�&%���e�!�	����#J����k�}�wCX4 ��I$B�QY<D�7�Ό�ɵ��L�3�����K���`�0�+��أ�4%eQ�`2X&���G�}�xN5nNIh���)��JCaOv  aȱ{]6� LX�9��O+��5� ���
�6	%(i��,Za#��s��V��ީ|FY�T�c�*ڄ2#�k��k(,���P�Ca�䙡��I�R�f�d��p��C�&Q��W�!�*@�IP	�Lɟ�%u�H�4�֫Ļj������/���H�M;K���io{��Œ͔�^�+�]��%P�
�hw،���:iyP���9�9i)�I	lً~`_�>�bM�u
@MWj�#�j�tk��(�i)����6I����b���lQr1�>�MwI��-#��at/i�H�p�*�&J�4Mau�	���r:�x�\5V;��>nQ��X�E����v��Vl�&�7%��R߀a|�!�O1Y�m��w��ߛ/�w��|U��>L�)�':�?��il�n��� #�b 7������U�4\�si�&t�8�7�N��eء��[����"��5�ehkĭ� �`�6c��3v���!@�i��4�(�1�MT1S=��gJ��"�I:y� _~�����<�u�|%���t���؊2mE��ӭh0M��iL��ӒMvo��꫾է_�X�Re�tѤ>c�1�Z�Jl0V:�R��ۀ^�M���T�L:��@Sal��˦�K�g�%��Xj�xF-u��$��$K�%,��W2�NfIIA��fI+ %�+E�~�9|	�c�<`��T��,��bt� �C.(jc!+��2�V�9Z���rt�cy?�����[�D1���[K���Ѭ��M���U��BF���B���^c!����RߟL�4�XQ�.s!LR�ο��հ�h�4Ƥ�Z��$?m�S�	rE�?�n>�Oa�XlM{Gn�r���[	��sX�"�0*y�k&��_�K��3@�ջ�O�y�I0�c?�Іr�C|?5�E��$)
]Q�M�6�j��I�HO����U�:��nuZ�4p~�����J۪��U�<[�.ˋت�[��D����]��٪�Nb6Rí��M�c�fí՚�$	2T�.�6�$���M���4�}V4~��Z�a�`��h�HF]�j�0�@�G��+R[5�aV�aR�����K=D#џ�\��3W�.��1t���uڐf���u	А������u��PND;2r�╿�����>�����4J8�#^��o��k�pR+c��l~t/�t� k$���[��c��L��c����@�F�x��f�}�A32~x-<Eelm(����� %�}S�Ҿ	wڂ!i�ș�Q&@�EP�I-�ȶ�CC/��M��L�<c� ������	�o�EIZ?�T��v����]R����a�}H���카8��%h2�� �
i#��r�NJ������B�I��F�!��AѯR��`
m��ߐ�[�I$8q�3��������TZ5hI1�=��~��J;$!�c�b���)�pK41���<���$< "j��V�LN>�4p�:�3����"��y�$����gtFٌ2��� �&�9]g�w�ڨ��6Q{%FtT�߼rT�g�%-�<�W�TN�����\��9[L���]�m�2�\)a��a`�"��Nj�� y5%h�e�L�3|��K�|,K�S儊�@e��M��F��	b}�
�7aa�P����jU�N��2��,���a�Wf�HSB�b.��s)�\�?��FY�2S5�y�������-�t��cb 1 �Q�z\��a�� ���rӅ��1&��SD���x��/S��9�T�j#��L�gRE����b	�;.�ty�NXQ�e���#0O�a��t;�
���ʤ�h���X�������Vo�x
�ɮV׾/|���O'��b�{�˓�NXO�2�P~�z�[��z3G��E��=�.��o˪�]5��Ҟ�V�Ҧ2P1~:��m�r0`�]Ci
q��0i2�[��%P��x[��f�q4�t�T�M����?�O�>�p.���e�ܮ����v��t�dz,
�S�h��K�/��@ ��0ڙ�s2��66K�F��MR)���)e�S�v�_��.V{�o�ܵpx�`'���~�J�	2E����K�e��NBŘ�J���MS[�P�
���E�ٌ�� �ɘ2'.��B'���u�qJ[��r��c�WϨoe�q��jnEc�IN#a�(wz����GS�Ȟ{!�v�ٍ'�4�TL�%Y�3�V�;f���À�� 5� �Q�K���cfI���<�8A ��]엢c����vPT�Y���?������j�S
#��L�X�>�<yé$�j@b�څ3H�����֌��x����8ݣ^J 摄n����)If���?� �d�o6�j�0M	Y���t���E++A��̷�]2�Zf����1�.5~���h���LY3F�d�[M����N�؍_����G�3��X�p3T�a���<_�#mY��aX�����ʉ��3�0������� CP;5'���<�_�+i.�q��}?�>��
7�E3 ��B�s����e���hۖ���ꚝ��$v�����ȗ��&��s"/���|	��3�9Һ�3�>�h�:���<, �p����?�!�IeOq�',�vf��%��2�\>��8�>%�Ћ���@0��k�5��Z�������}�� 1c#�>C�y&I�bk�@ڞS�h�?B���F˓�-�
 �`JE�l�-89@k���q��mIQ1ף/�v�$�+�Ւ�k��f�����W�
���o����S�.�)�l�4��[�+ER�f��N��Dx� ���j������� ����\�ӀZ� ��*&�1aa�FHǄeH#�a::���(���!J5�qF�j���D&�覧����}�i$�@�s���Ƌ6�����M�\��j��g�,�OX�hϗׂL�_32Yͦ��Q�^�4H�Q_tH���b1 �H�1*�6옢�vL�v](D��B�4,�-�����ϱ"�� .�ll���������9E�����
9��!��0o&mƵ8%̛)�l?%�;!�)a��3̛qhM�(�[�0�P�ü�Eٽ0o�m�mUd�4�����ݺ��7�!_,n߮֏����0.H�m��2��|J®o��-^M���AvG�gNk�����g���5Rٍ9��R������C֯n0-
C�.��N&��fE#,_q�u�;/bp[4S�r��(6�8�&��z��W3i�<'�54\^�J0���1�803,�r���DZ�i�*ocYI�T[W�%V���muY��Q!���|�/D���@7#��ͿZ�5V"��fh�ǕV�]�q}f��b].��j�������#��@�J�U,  ��3��!Z�m����c����]C�9����������O7��Qǰ`�rU2=�MĶ�lc�N\>�H���3��mWl{�V���<���ņ�0{�����!|�wS5Z9��;#����F��!�z@�9��Z����< �7�����E���M_�a�6�������Rm[RP<���ԠV��N��}�z�SXy!&6%�\���gHSzNLi }��
e��N�/
o��x@�K�r�Xp��`�[N�q�Hk`=��ء�����:��X�*F�&:���E[R�t9��#߿@.0n0��I}^�D����r�<?�F�s�
�Jw��v��/wv���D��56i���v>c&��3r�J?�f="VcCa75�����B�!-��[i��De<萏��ȆƂ�*\��XC@�w�2�&&��G��q� ����ڭ��ߧHW��r��K8X.ɚ���"Չ<�
жw�pE:0P^ A�B������A�V���ѡ�����>�)i��`+�/�vhD��6�-�����#.��G�nr����'v����Ps�#*��uד��Wu���:���W�����/�w�����(�b�CahJ�9Z�PW�k�����>��ok|��UZ�
��l�
�F��:����Nd
��Y>����v�^C19�/µn�\<��>���t�b��ӗy���R4-�������ԩU�SH���� ��3����)��d��6�1�;����Ɲ.aE!�t}"�	H����F�t�R�P�����?�V#~��n�)b��3÷�B�Ƕ�8�2tI��*<c�����<��IR�`�̓���"
�;ws�+�W�Q77B�i}봘��t4�0�K�&��jU���f��\�
P��d�Gj���|<�M�r>N���t)U��I���8��^��\�a[��������?�	&���:YK8�up(�o�~c�~dF�D��%;d�$şҧ r�e��2j�oCm�E>ۦ�馆[��o��%��$L՘飱��tL�j��NϘ�l}�K6�3��t!T�dI`�Yᩥv�:R�]��+���Lu�*Oix?������`!�XZH��Ȅ�nm�i? �;�T8�A���(��r[ D`F��2@�W������q�n�����5Y�>c�:����)m]��MQ~:�HՏO�|������(��xZ�f�?}=��� �иŴ��^���/g�?��Ku���̑Y��L��^ͷ��q�)����x���.��\��\f��;� >�����\�B�w����Eb֪؆���n1�<4Ίp�%rG�@��D.`�#rG�	�ie&���Y6 ]0��S�EL�=|홣%ϕ󓪌��g��׷����Rŝ�~/����R^Dз琢:��R�J�C
��R��%�t��6�f\mx[�v�3�s���:r�D[����Y�Q3�K/W�_o����i{r60��]�R�8�����3�NNU�*9<��D��y�l;�4E���N�Y+^.�SM�l�l����S,Dy���n�u�u�(��G�c���]wb��~��_�~�g�4z�v��%�(nON�v�rS�K�P��i	�b �	(CЎ�E�K�)(��A�����Ga9,O�&��v��Y(�g ܾ������O��#�Oh
$6�����1��e�#�g�ܮW��Y^|�M�]�F�n=}|�/������n�.�)~�u��E���"��`����P�M�8]�P2��O����|���EF�b������ŷhG����{������u^���w�M���{?h�~���>c�E/�b^8v'/����"�ǯ���ɴj����	����J{F��rS�%(N�+�8h̗.��8����v����o�u��%��xU�\*T�8���*i�D���'��x�͞�& ��3��c4����V)ty��d��׏��������1E���g����+��b�֫G���êԛ��P�.�-C����
)	T�	�`�ɒ',35Ni�����*\��vX��7�V��;�)�ง������x|�i��~,~���)�4��{f87B���#�L�23�uf��]9W�朥�i%lX�m��dGhXȦ �Dd�7~����j��O�3���1�N��������t��C�	�ϗ���i��nڝ�j����Xoa"�nx���D� �^�m̭�_T�I�⮰�l���{�3\t�����i�$]�x �l8�aL8����n�]OK!7��)w����1�	lm�8O�u�E�ՆO��L���Ӵ�qi2�T3�qT����7�ѧ����Gڏ� �?�ϾQ����&�i(�e�gl\z&�!�F�ܤ����:��Da�����e�֕!�J9��H�[�ԺF����ž|����J���t[� �Ĉh˴t+Lծ�N'��i�������2+�����:�;YE�e�z'>�󱲆��I�OY��kQ ���'w&���@��_�^����x��#�D�y�f�J݀�@3������6�$�N~ܭ���D�����[��wB����n�H��]u�*��D������"�F�;@�T�<˸��9�����Y'��jp�(2�|���e�(�Y�m���ӻ�ak"?�M>���?����~�J�S�a�M�IkǗ�Ш�t*I�iW=O�Q�4X�PN�V��=���f��,��(&Wk�����#>6�2p���9�H/������d�6{��3�N�P�X¦��P�n5,a'w��g0�-c10�}�3HF���!��ɲ]u�Nf�GT�:2Jf,^ח�4�l�^�KW�
�ᅪ��&Ѝam"VS褤��6��45��M����c�T�7OS�ԇ����*�2/N���w�c�P[-�H���su���?����@����g���)�x�H�
�2s�b�� �(��i��.�ہb���y�@)o��/�P���?QL�^L��:��䐨�')x�"-X�*"��f�z*.aJ������f��hZ4)�ь��W�.ZR��#t���
�D�ʒ���dR���è�+��67ɐ_䙶K9���ِ�KuVt�3ų�2�-������2��0��J�)�X���((� �꾯f�u�*`yoW�nfKá �@�X��s�	���$��%�*�r�	{�Ϛ\�����V�	PHh9K��`��84U]1].uK����ئ��Z��h3:���<���=hr�2� T��~���u8��ڋ���_���� @�y=���n�+2���5���4����6�}�
���5�?��ָ�|�����<�т�mz�c��N�~��j��������#ų!���E0P$�6��vW��^(Z��~��[�>���ѧ9A����}���.��l z�F�U�ԲPa�nђ�5�dц&k:4H����q*q�L���3���W��vƆL㜓Fd�PUl'�\G�7�s�҉H'��������b���[c��O AU`�Gs���!�����zW��1J06���J�Q������Ŵ(Wn����&��E2F�2�_�׵�sx���VXg�b�օ��Y���%�L4Х��ڢA� �����Q���j	��_�w��7�i@��'��kRy��:�9*{;�Efls��*�Vk9�ui�jRk���B�8���V��:F�	���t��e;%2mͳHEA�PN1ޣ�f�	���ZZ���hs ���B�t}R��d?�P�3��` Jy^9�S�k��.���r=G�:���4�21_&�GT��p�ۻO��@}���ٕ�-x����{���ϑ��mi�'���rt��>��˧OO�y~����~HpQz��Q�e{-�7����}Ω�+_�q���(�n�Æ�R�����fJ�[9 �,n���stD�-��w�S����m}�7���,�A�xo,^y�L����]����^�6U?p�)�]�p�-�G�n�,;��,�1H1M^������o�!Iſ��G��*J7��-
��	:[s�'�����޴8�v��g��p�v-�`�8��C���;*k^��h���ӾjYkB���g��nSd��.O�|/�*�=�D&b%sS�Ԍa>,��1�d4r��]+���P���ʨ�^��IAk
endstream
endobj
55 0 obj <<
/Length 7919      
/Filter /FlateDecode
>>
stream
x��=�r#7���
��+��>��113����ll�#���C�*�iS��G���7@�"ꠤ���>U�Hd&�B�\=\�������}�͟��䕩�R�����"D^i"+)������OD������.���N��d��	ղ&uE	��$�j�C�`�
��Ҵ�Ph��Jk��(DT,��c�	VIǰ�j�m]�6ԍ��P�$5��G�Ւ��
�+n�cy�Tڶ�����j��-u++at[���%3rhi�_N;��B�ʓ�VV'���(�Ƹ�:4����t�Te�x��ɹ�wZ:mY�t�F`����� K9�������XW��1X]��)4�Q��:lB��Z+# �T��J�a 8 x����ښi���q},M�����u֔����f,��|L��<�ԙ�։�����#��y���Rd����b��JhŨ�Y�O���JC# M�1�7%�w\R}�'9jD�9:Z�����_�	:mg)�d��Ie�ȇ�����9�F+͇�xgk}�ݻo��n��b�����6{�z��ǟ������꣫�x%UE�ts��7�D�lH][I�� �+y�a_�I����rb�B�a��&�D�`���X�V���~w��b�k	<*��&��/��E�S�2M���2g������)s�b�n��eQ�i{)�� �f
��z�D�1YY�4�3Du~�:ZQ�����@��ԟ�zQE+���W�-�rŶ�
dlް˙�b��]��L�REjɈ^�ebT��`��	͛8{���6�oV��CS�����mQ0�"�]��6��5_�����sf
���87��u�c��F#�4��w�~7�|���i��j��M����;f@j2��hӬJ0�ċtP��sa_�,��^z��P�`T�6}�k�"�D��m%m7��B'�Rļ�BM_G���ꬳ��"$�z6s_U�sE�_��_��G"z���"���^�fr�0��j����0�DK+4�j?-��{Ih��lr
��+C��D�MZTD�+)E�a'��~��!C@ے@����L��g�Rb#G{	d�#-Ȝ�B^�Ԛ�2�p���(��_}�)�0;g���Mo�s�'����|f�@���`�� ��oΆ�k��6|�,�̍n�0��9��H��J�%g&��x�*R#� �.�>���B�8�2�*p���R��:ƒ�t�'e�z����M����#$�P��n{l��\�V�a��g�wYߺ
N��6�oOU���[�,u>��L�A�,����}�7�Ě�h�5x�k�m�;���u�$"��	8Y�J�3�t[h*�	.�{]��
���BY���GF��CC@V��f+
�I���S�)�;�ٳ��A&����K�"Z�i���$Y���%?)�x���ʀ����d
Efv���L�:���!��3�:S�����l< On�s��>����Jq%���<���s�Nn͘.(c	����JXQY�|�_?���Ԁ��IV%�4N�h���5��zs������meXp��{�7w�U��o6�8�6��j	X���aeT����à8 �O�����?�w0`x��������K�}�{8B�������O�������l�����T�k6bJb�z�p��0	j�������_��V$�v	E���-; �^�f7�mS�='�������l֏�m�Կ����,��9#:~z����G,��ĥ��o�����i���X,N��n#y�|5�&m}�&�������<���nlď*`�	W�Y���p>�w�0�,�;_��~zj�����k� ��{����K�A��m��]���d�[��m��J;H�<:���58�m����z���þ~|����&��Z�EU�N�ф�	l�|إCY%�C�͛��#dqfK�[kSv��ʤ�既�Ii{�֔�~�sV�^12.>�ۢA/E1XBUʪ�v���ؿ+5���C��IS�#�T�T6�+����_Yó H1g���=u��`�Ԉ̈́VM�_�<�Ϥ_)�H��_���i� A�7�K���16O�?��rtucYf���&�w$fm*$ڈ~1krV���YK��$w���.{���Nb�u�3c&�nE�SR��X��q��}}b)�X����㶊��o���2�twx�r;-����	��C ΆE\��3�k�5A��FOq
1c(���`����Ĺ|Zt3���k���mB���כP�AD;�U��D}{�n��?y!
����LWu�S	�5@U�h��lu���q��@I����\*@Թ�m��"�SnrX�qD�5`1J�Xvl���U'�b�]���;���qnE}�#+�8�Ĭ� )H��h�袏r*ЕV�
�UO(kj�K�e�ҹ
����لA�����A�Ѫ�_�%�k�;G^�[:�� �=���kJ;��
�{q�H�aF#�]^��{V��mo�M9Z����P�����\�h�����i�B�FK������uiR5��آ�\�k	*J��*��:��8���@��u}��k��h����cP�u�;:�������=4����n8��_	by�3}fw:6�ǝ�� �.�705 ś��"��ŗ
����Ekd�@t +�$1��	%�mW}NlW�b�������
��x8���������=s�W?�_�j`Vۇ7������E��y�H�<�i�[aKK����~��g撾���h�hP�Du�����������@:�:�L�Dn��}|2ޣ*#�nK1��]�3(X�p����Xt��=��84θ9@��0.[���ʷ��S���� B;�p����@�eQ,�F�s[�X����j������R���`�,l��4���#�>�_��>4�X6��YQ�� ()�����������g���J�R�8m����-պ
�/��n��pP8`U���j|[f:b*nD�t~GP2��FDA�|��Һ){�LJ����X�A��|a�ǡ:�@������d.�J��m���8�1���q��o�P�H%���O��v����y��k�WJЌ�/4��v�P�����x�_;\��%�ar�����i���2H��[��w�=��?1��M�~��`�p�ꔣ��)�KF	�����nΈ쿣�ߙ�QH~l�-��h�]��s7ۻ�F�T���:��i�E@]q�~|g���?~�?�C�z�$8��P,�Pt�t86���o��X����q��m��M�H@v��ۣ����O��I~	{��B<p��`m�|Fl>��ݶi+{����6����V�'s�����s\���(~:xӊ�� vwN߭������i�F=�	�x��2��ޜtQ�l�)���< w�!�CE�n;��j�G��(	�M�[1r��_M�=Q��Й�m�HĀg����d���bt������#V�.�p��	덧J�I�� W��a6.�ڨK���D{{ߦ4�T�]n�[h;Q�'�B�{Z/���L���$�0V���l��{$��f��];��*h�~$9��,m[���� ��Q�w&�ʿ����jph����i㋒��״�h+�����Z�����Ms�_�l�~;����O�f��f��7����Lq4�q�A9��
�Eq:\���3L��v:PVo��	��*8��IN܄��v_oW�_A��7/;|]ߦ
ҳ�]��Wi�@ݛ���[�q�XM�噡AyE�UT�sv�0��^o��)Lk.t��|�1"�2����y�dl�衟/���h�Z����9� ��`�������Q ݎ����Kv1�L�\��? ��W�m�ֱz�p�m����rܯ]o=A� ���>L i|��?���r��งQ��u����aj����G�17�e��E�TY�Ij	��^)��V���YF�vϰY�(���a}���Y����ʂԅ�A�x\�6����;t_����4k�=���?:V��a�[k�q�����>����I�/���V�����^����-ղ���V.��X��~����~��v�7XՏ�&4��O@�m(�k�K�2���%��S�X�e�;��K�a
��)���Zk*���'�o���QdT�M�8n
���JȰ[�g+�{�������U����Z�V�m���ʡy\�v0C����f*����Wy[t��.��_�&��uS���q++xJu��]�)�K��S�sb�@#'#�KQ��N3�?�\N�O��ܼ͛jDs���ༀ�v+��y]
>����oE�24�R>+	���[�Å�Qp-cp�W�e�n��s�X��)�RB���`Sq7�$�<���$��#t.�G�Vc����,]I���Q�%�����ا�;K�#��Q�[����1v�o�Y�_��;����b2 �J)uI���i�BG��QF��B.�(���n�1��-�@N^ �%:�V��Y���VN��X�98�8qW�����yiv����-��������m9j4ƪ��̂P7n.��~W�Q���S�Ll�x�E�	��2�ZI'R�]Jb���OI؄V�˩�����w�7��7voJ�����tY�Z�Y;�[;9>i�d��n����d#��	^k^�מ���w<�� {)�rJz[�v4��r_����%{IɋI��T�)5�v^��P>�m��b8/��x1��tr�ˈ8�Ƣ{��i�1��f>�@x1��򥐒��8�-`y?��Ҝ~1a�3��F0x�s�p;���������d����a�#2����M����o ��8ǧ����8�\&���$�h'��,�������=�C����Q�t����h��K��H�'�@�^�j��*Fm��#z�ҹ�g�`k�b�(�̽9�5&w��8�0���h+�N�`��ϻ5��W'e닼O����~y��<U��O��q��z�	����u��r�ۣ��P����;�0��˪w����|��e ��T$fRpN	ü/���=`۱�Y>��lw? օ����ᗵ^I�����s"��$���؈�I�W�ў�@�ֽP��M�Ћe".Υ=�ّ���5�o{��6P2g24�rO=�^���̦d�S�+1We*��*C�gg�lV.�Ic1
1�X<W����2��zqp���=ՁE��=J-�&����fqX%�Y�����>��G��i?�9̺x����Xt</Urc�JEѭ�-&�1q*7=l����tU��C*��aߔ�m�Y�i����h.5$�����2���yBX����-:Q���qƛ�1铈�r:<��댈��'�EĴ_c�T�c󻈿��_4����2h: ZH�ga��1}1�f�!o�˭6ρ�mQ5�j
`�R��#Ԙ�G|E��o��@�Y���������~Q��	eT��yK���y`�%J��_����x�N0���l�W,=l��n�R%`}��3|i'1���6��+�2Xq�jh����Զ��'���6���:K?z�i����5׋�o����Q��0�▷<�1���Bd"%�+�I�T,*'�'�a0:�C�G�옗�|sR�}(��$j��y���ğ,����ҞYRQe���5��>et_�ʯy��g��+6�Me�EY�m�\��GdMǤ�/�J4t����Fo]q$�zU�6Ilނˡ�	6o%����.;�enEI����`�$���C˔Γ#�>D���z^��z�5�~�?��ns*0w#�Ș��Xu<�g���H������}_��ig���ݞ��x��Q�q_3+���'o���ϟe<��灳Nb2o*�'r^��F�d���%'����s~���_�ǯ'�����@��.��ڙ},�VB�g�m��YE���A5��'�Kn�e�cV��@�i����>��猊TJ��b3q�� g�0���mן}��"�֥U_�Z�����|S���WO0}Cf1}s	ƿ"�x�)�*3M�D�Q���i� ���9�g��K�p;Ŀ�Γ����=X�$>��u&����k89��l5x�+�1�"��dʈ�����e����NГ�N��ųd�����4w�w�����u%f��}Ao9�
�����Y�)�s�wrM�+\��:���f�6��%��N�l9+K6ͳd���y�z�	��B~��>	����e�S��FtB5���4 �����Ԛɭ�)��FyS{Ǽ|�D�,�t�3�_0�Z4��狌�l&41&�l�&"���������C��%+���y����p��O�4��~��J*0=�<OO�]w�Rﾜ�އr�o� *�w��xw���'^C'��~؆ds����.InI�+���O��KC�T�'��
:�YS�5��ux�<��tіc�8�Q�]F9��W���?19��j�>d棼�3.ð2�g�/Fp!
��.�	|�T���6d�h���?j����Q#�\6W2o@�ͺi;��,��f!x���v<(	]�+���ǡ����Ƕ�ʧ�|�ֻ�>�Kg�W�<t�`���7�[��uqh��:�N׹�n���"<1��L<���n�bXXIx/x1Z�U��uU\�������c����C�2��1���b�y�%M�li��hE�䨢p�V2t�q~�t�Y�G�V:�ï���r������y;g��ڎ�&ڶ���b�_�!V�m����fƊ-L�*w+uA!��M�m+�ݟ��-���yӓe���X�G�ǁ~�*j���ldz��ƥ�^זf.�����5�$��׏�Tt��Jo�ʙ��?��������ǳi[�wJ6A�'�^y��u0���������#�44��ŋ@}~�{��Jr�l��Nu�m��]��������|G���kQvO��?��vM}1� ��w��h�;���b�AA�m
�Q�^�g�3�_�O�A�z��$5i�v�p��=����	��"��H��໖f� �ް�E����*�cv��
-����0�ۛJ�T,�7��*_��;g�S�M��T�"��|Tq�b�/'w+Gz-��V��:�S&��ʇ����d��.c��s���T���Up�hk�r:���X��SE{+ּw�L^��cVAz���ؔ�Hm��Z��4�Jw{�rg��bכ4�%F��Kd��:ܒR�_ܱ�.�1����*f�j�/K덫v�O�`\uH$�g��������8��]���.���g�H�Mɚ���;��#��<�BX����h
�X^Ĉi������A�A��P���5=�}�����3���t,Y��u��Kf�˙�N6�	I��
��N���Ǧ����e�VL_�N�
@o�A��48~b����L�^C�1u�!�����_ŋ֒k�2��:���d��j���M(���X�nS%<�dɜ��&��ܜ$�s���:�8/'r��Th��~��ֶd��堗�>����m�� x�І-hּ�@z�Y� ��3��� �ͱ��m��z�"$��2$?98�TZ* �9d`h���	�q���'C?-[��o����+�tw���ӡ�3j9���
�WQ�ͼD�-i[ɗn��nG<�w�4��M7A�,/A���	��%N'�fJD�<tcL��"�y:��
�9�S����i9�2�C1��������I���*��I�1���'j[�ߊB	�@�i�E��ݜ���H�f�Γ?�Vl�sa��S98D]r΍�?�U|�Lt��+�*1�-hGӣ&ג�"���	h.l��( �¼#�T W2���T��v��X�U˘�����De�:���ZUPN
endstream
endobj
58 0 obj <<
/Length 7634      
/Filter /FlateDecode
>>
stream
x��=]s䶑��+&/W�*��7�M�R�U|gWr���vU��{4�̇���� �|h���^$Ā@���@�ɇ	���;�s��o����4FMn&ZM,���������FY7]�m�������Vؤ��E�t��Ȅ�tu#��}h����oiE�V�?dz4W*6�&Ӎ-/c�?fzP+Ml�)Ӄ*�I���I�Ю�5�Z�c�Ng�����>V�X�o����sMuT���v�+����jQ/���*3�k�rrSBUX��D��8�C`�����]4YH�B��F�-�l~�?�:REY�v@7��i��/�X3��2�By
�`rcx���������jͦ�jI�������y\��'B,���|=��M�����i���_������rC�0F�b
�@}��B��F���驢>T��~�J������!�bY��(Ӆe��A�3� ��e���3_q�)�޿�tac�YUX�n���l�e�#wƌ����ݿ�q�c>�%�Pnb�,�1��ӻ~b�{x������7}Bv�ZL�����v&�teu� �F`d_!]H���3 �4�ri'7B�d���J��j���G)�]ؖbN�w�@�ɻ�v�
���
Q��JO?=��y3���� ډр����.]m�f��n�0� �3c] �\�^���8��863�Oc_J�����R�"'��<#0��b�_����	N`+�: �C���ն��ǻO�EyX-Ĺ�?�+`-�+�	y���# �T}������?e�/RQ��[���\ �N�� "�=S�:��Rx���ȆO�f�W@� �:3��,����+5�?��z:[-7��zM��냵�g����f]����X�z��y�эO�#���y�t#���7-dM*
�5~����/W��<�0��|�%�m��7���D'���B9��j;P,�7�#�,|��[����Q<_���%/�);���-��T-�������b>#��gR����n�V2\�^�'uM�wW�Mw[z���~�g��/���@[���i��,o�fd�t������\�y�µJM��YJf ��c[���4@]v��1�.7�d�����!|r�Z��(�O��@����V��j��^��:_-���t+���SD���(RG� w'���e�����{{�D�U%\�XSXzk���ٱ��2@Р�sLZ-Z��#:sRgߪ�&UOwCL2�4؂7L3�g��0������C ���0\�x�䬒�Q� Ҡ�=0.&�^�B��zK��dG����Ь�1Wa���ݚ����z�K��kG����:�-�8�ٖX�g�(ށ6 ի ލpb ����cY�Gt��f��P�~1�	�YQ�v����5�d'��U�xБޏAw ���s������e���Rϲs��{�(��]�i��~KB!���Y�AWM��=����]��K����k��u��F�Ss�S[f���|j������y��3Z=�A��j��8]h�>�o��w�r8 $�2�\`���
g�ܿ� ~���P|B�ai���AG藶e�����jQ�2�X#�9|�s!� ����긿�W�P�^=����ķ�
o�J�h)��v�a�-ŧ�1��j��P%�,��h�o��Ê��K�ڠ�J�^{���ͱ��zuL�>%K9�{�C�r����Cmx�.J����/A>@��㶊Ɖ�>v��%��72L��Uya'�ٖ�3 {bQvLTV�q��[�f���%�%σ���;�|_q�Ǌ�W��sfL�0´�$��.�'��["?��}����w5=�t��QLt�ۣ)�~�n=2H4A�Xo��J9��^y+Z*�P����'U@$x��$ql ����9�jݶ��,�C�\�=�w��G���E����>����z�aQw�7%0�y�.��5�)����� �FE#��Z�0�u��}��m���u���~�@�u}_�_S�nm]����؉l:��Pi����	& ���zu����\�K
fCZn���$J0�oX��
��=|���oc*8&��#t�+����Mge\��i:y��@5�N��NXdV�!(��z�C�\��W��^���	�d��Z���㯶�OWF{#��} чQ��"�42�#��t��AV\�Q}��� ��=&T�剖~�^nv�9�X::�M�/Z�pZ���*�* �P�Gds�4��zk7��	iε�hbe�Bp�C��Z�������l��+�>
̃�v������P�q=������OT�����s/T}����;b�F
�
`v�]y~wM��B3�?�/�=� K݄�$2��5Ȋ��_�](��/@ǋ����T�~��DZᖡ����}������V��nI�D �kFOީ�pp��@@�Y)XK�Rpb��k�BL����kHdE�+�a�Ԡ^̟��*�{�����[��~'�A���ڮ�E�5@��%�u�حǿ�;�(P�kr��ET�T�$z����Ցt"�)|RS\L7 $�)=���@Fo���Z�~��N���ȉ�U.R��Θ
,}���z�U�V�&�u@9���[��Ć�V�" g>��8{�-�+�k��(TG�g::Q��Gd(�ҙ���#��LY�۠*�FA��B��E���=>@�66�TO�����f3!|I3�Q%�Oi[F����	���ϝm�u�+��5h���uM�6�� �K7����+qr���+H|��x���_����
�u[(�� �ʅ8=�vkzm(V�ࢋ�Lx�ރ�G��}�>���4�I�E��Y:��3��d�����s5_o�����N�cD��w��ҙ�Ǐ���Y��;�{Gc �@�~�\� �\!Mw���DQ�[�?�KZ�<H~�8��eC��;�4s�4�]�[4����\�Y!H��C��ކ��@��d���VydD�19<dM��XFQDn���q�P����&����U���?R#�	�r]Q�3�`��%�mc4�&g����S]C �'�%�<�ʹp�r.\���x��������k^����5�oRI��($��)T{~,M��Z.�E��	��Yd��D'�/��E��]�����5��tff�U�͆�VT���*��iIF5r��A�{a���5�p�`���#4G�_	��T�o�XQ��$���5��<A��i�zƨ/��[r���%y*�$��N6��z���o]"�ˮt�}DD���N�s���]Uދ9�����5��#�|�U��	uDˀ^�s������}�:3�[�Ɓ�ڳ�N�=!b6�ĘL��ЕO��̈́�n�U!��>0���Θ�1����HF�
����+���>V8����g���:��A��	�것�Az�����
8�X��>�K+g�a���C�w�]PY䐅m�����J�/O�?�.�5�)L�%8�U�Hv�4�4������s��F���(�!����ǅ�Α+�nM3v�Ğkp�	h:HE�T �N��5�4E��1�b@�;�VL4�d�
�� )��(mA��¢���;�����eS/��/!R�5z��N z[BY. A�T=k�ǣ�5m �r���?=�\ˠ�0M�['P!�gַ6굏�C/��q5���<���IM�=d-�¶�C:2��������rLI���b6�����*�!�²�oεqi�.9"�k��(���,4��� �J�"���]E(\_8��Fu ���D�a����zm[~�@���s
Ս�*�%K�]�[ٟ�'J&�[���_�B��4H�dtv�9�+:_��}�����6�x� Ň.�b��bQ&G"��D��n <m���Z����z��D�J�c�=���i��C�l�!`{<# ɤi`�,%+CH��(8�9���ݺZRYL�v�&����D����O��8��<9X^��Q�.'�Y,�G(=8"¿F���:L��K������/��Σ����`�bl�:o�"��*:	��� �˔z�X���	�h'ozg"��j=K"v:��nY�0��m�a�hI�6��4
�W��.7�{�DǤA�z7�ZT�t���n�7B���iOj��(�gl�	�T-�ϻ�T:D`#����޾���]'����n+�b�c�9���?`$��5�4���q��_���1Pg�8*O�6a�::h9�U��Th�K�.��!�Rz�Y�>4rbK�@Զwd�B�r�ʤP�X]������9p֒�t������f�=i�ڃ�>T$���(j��p��ہ��xH��R%��*������}�԰V~��}Q]� d�������Z?���� /�}|�'`�=5����C��!�ˁ�5x��@	V������T�I�sx:�dCP CS��wI��1�Ο�N�����c(u�R�:x�?H�4Z�|\oh���֙�r�*�o��+��s��v��2e�ˮPa,E�KI�cM��y���#U9�|��c'�`\�~%e��@l�3�ZsΘa}W�����*���Μ5��}��>;��0�ͽ���pl.9&b_�A�H��B�����
�@��x9Y��<3�.u9ش�@CWqV�W�{�:Z$��2ɗ/��Xv�;Js��7\"��KԓȂ�� �n�R�JdU�3�A���0��䩽 �8`�sx�������1~��>jj��:��
tr0�
/�4o$��^��[��oT�P�U>�u�"}$�WX}���� О�/���bDR�cd���2�|k�z<d�UA���9 ;���b@3ƀpٵc�Xh:}>,��s/��8�X�岸<�����e	�K��É#����e]��l��x.Eg���0��(���ȣ���4���he��Y�"
��ԈX%s�Uz;�;".����wYW'3��W��7���]
V>]�~?`Lr��U<&�P�R���ʁS0P_Έ���s�qp�eQ��-iW#��4����/ë.�b�����$��7�B��s����
o��M$܄����y�v}���b�*{�i�M�/�b�p�ˋӽ�d`��x����U@��55t�����_O6gLQO���qgX�R�Q���m���+m���E��9���{���C���P�9�'�w<p��g��F��=I�idڐn��E��P}�gm�Q6qb_��b�*\&�����|�c��?Й����T(/���BnԴ�g�;��[Ǐ�2n{�^�-k[8P�f�+�([�xi4��@�[V��|����>U�Y�a����]�����_m�K��W=�}�8@/���v�t �N��w��v��_�
ƌ��f\��;6�޾�1�D>�@�Z�Ayp���r��9��ƪ�yd�hsVވ��ٚdG+�zȳsX���<K6��mA����W�1%��#}�J]`�*�.��0^X�P���G88��ey (��D�t4�{����"@J�ޭ<'�gy���Ry��������$N��sׁ�+�>�N���k������w�ͽ��Ri]3�k���,G]{e�qG�h���GȀ���8���"��!Ɗ6i�O��8^���Ͱ��8/^�/�C��f��i���?@������	0�眾1Y=۟�i�bY̧�r�<�UXy��VQ/܄,���y���Rt�o(�T�U
��m�`\���[�z�S@G� �q\�Q��t�'��eg�������~خm�b��dOՔ-<���Dg̯�o�:�3:?F3n��R)1y"�|)'�x�����G��'0h6�~�����0�s�N�9P�S�;�sv+LY���1n$jE��Ju�����)�����"|Y��<����4^v�����H�g.R�y��~�LW�Z��Rt|��P��P�!�!�f\�:���gr��	�𜿐��X��##�fw��M�B{]?神��>���Y������U�;��%��U.�[�w%R�����+�M����Co���E���1{N�{N0�R������O�Uh5�T=Y�b��mϩ����4��yh�i��Uz����&�ǲ�q��W�U6���ĺl&�r�����ŗm��^Ʊ���j��|��䜿�5�Fm}�D�L\ى4�ϡ�:�?�+��+>�%��#�9y)��R����R�>��l���:���3y�����-��fp�s�1;�,�%�����}��1�^�s�A;{��u��w���%JȌ}���p����o~�2���e�q,�<�'N�k]p�m@�c`|_j�/���ԅ���V��w�|�k�.t��M0�wp8 �=����^����[d����I6p�Ľ��ޣ%y3��hUzLS Ye0�H��)�o�.�T<7�J�޵h�o6yosWnZ_�����-{���ն��S���*�������L�:Kk�&�N��#���&4����d�[�y��?W�W��}��R�8>p~֞sz��p�9��f���
~��žV�nȔx����Cc�23�4���&�ADzΞz�4�'#��T��>�S�p�*�U>��_�z��t�����_{5��\V����75]a�H�.u��s�@�/r�Y�5��k Q�:��3r=|5�a��G&,�)=����	�1i���PsX"����@��˲����')�b�$V#B������yn�7E�W�(�]�rdRz�DW�\�>���`��I��R�H���Ŕ�L5�Ze��*)G�I��6�*�m�d�>��V���:�&�}�+�q$�Lb�FIQ� ā��Z7��k� i��svŧ_�y\��T�����'*�Y����^�̠8W��_/CS� 7���ǥ��|,),���R9�B���^�E�:J��%?6^6iܫ��^B��8������Qb|:z^�iD�n�i�j�t�[�P���!qE��<d�����n�Rl�R��3�O�,~3��]HE.B>ҲR��
��j���U��o�C@��Y�7���=������@��6�.�t��4K��҂`	'I�	����JJ���H']Kr5�v��������Ow�h�V$9e�L�.�4�"�m�F�K��z�̗�$2MG}M�(I���AE�=�שb�J�Lk�ľm�:��4��/���tL���O*�)3&�o�~�w�?9�TT�[l�L�t�����m>�Lf�&[}�f!�(=Pˢ�z{��p�ze��ӓ�E�SUd����v������k��V�u>����Ls��c�*����C��뺶���	���s�;�%{Df�$������<��i��@ <���2k��;�Θ�x��r(vXr��ֵ��-�q���b�/�J_	A�S@r��H�� 3�{\�ug,4U�6;5Șe3-���lĨ��N�-3S�i��놑�����a�> o�%X���`z��sQ����`-4��f('�ت��<�F�K\�8�q�%
���������1X	�\Z@I��xo�6SZ7o��m)��-r��W�+�;����q�a
endstream
endobj
61 0 obj <<
/Length 7930      
/Filter /FlateDecode
>>
stream
x��]�����b�%� 3����.{{w	��Cl�f�ck��f��=_�_��&[d?$�x����Z-6Y,V���by���e��޼��Hu�#%�xsw!��"�(�ś�������b�^��?��S�;�~DE�|a���k*�l�_Vx�g�����n�^�/�_>۽�܍��b��������}��.V�ܷ�1>�xI�l�Y��[��h��uq�zpm�K������=l�m�q?�������~�u�/E�
r�>�B���ꭻ��g���wo^���h�rA�,U��!o�_|�Cy����EY0�/>�G��,JA/��_�o�	C%4��o_�`.�Ђ��!ׂ��(��Q8�!1�HM&��^��kR����z�z����qw�������c�h��5a���5��8m�4'�2�G;��ya��yg��nS����%p���^˂jR������R5����5FE���AI�
(���<l��B0^?'�%eQ�M�/M�B�fo�&x�����$Z�1�+<���ݵ��9L�AX�L��!o��Z�܄�m.��U���X%�fh%��wM��RNP� N~��0S���g*��	0�D�i��Wv�g�^�S7� &���
�y�D�)d�It7j$^@`�1�>M*�4}����
�|��:E`�uI7۝�)�	-�w��$��"�:ADc'c'���ÈU}WT�ܕ�Ų�*<���d������f_�n��`� q�)X)��U�g_�jX�4�CD��۔$Z�D��S�'���2�,�-�fr�%1�<Z@	�X�u@@��H@\�{5���m�$�@Id�h��X(�)7�aDA�Sp!vB �:^�LF��� NQ�С�)ҀN�Hٽ��lw�� /ve���q%Ď`rHT��x�,�� 8��qm@��CCȏ!?���a�`��"�M��ZB�N�Bә*�.`:M �������a�����_��r�rQ�f�h���d�[i$x���w����B��U�2C�W���#ב�_G�H�I�����n�}(�M��������_!C�>�.��rګ�*��'��H�@hk��+W���8�$�E�S]�Ax	ʁ}-&�`�?u
 ��)&� �y�	2y�h `(��ޑ�d�)y\��6��EN>hc��m�~"3H��@>i� ��B�����{��	��a}��E�C�-
���~ƹN��� �p�^�V�.�Z�Dj��wI�D[�!#���
�X�і� i�1iIC�z�N�<��aƮ"Eb�����h�5��C�$F�'k2٧��iP��D�9x����~g�D��?�K���Z��}ؙk"���Q�@�z��ԡ~�]�n���h�%(O���I��&EjP(���*Z����8֋	���^� ��h���r����)��N�L�Ώ�B��oʄ�[��� �{ɹ���k{�f��sao֫�>α[w���u2R���ǋ��`OLV��hAa�	X��b:Q�A���J�R��F�\�/V�^-����Z�sի�uQ���S$�4�@�x,iX���vN��j��=��iUƉ�c�<ݿp@k�Ȭ���#3#T*zk�����:�m�N������֡Q��
�����Q�����Y�PF�E��*�],�Z4uH��"��b�U��b��V�s� �#E�B��v��2T�p;�s�	LΡ�;�ɨ}֪�����˺*��9N�PE>� ^z��Ut{� 5��0|�qʕLL\ibPQ������\%&��i�)�+���a�#��衘c&bTU>I�������N�♉nr�� ��gp��1nQ��B�3�c\���g�0蹿��7����ϭ�D0����ro��`�?^J1�2�"I��3+�1+u�t��r��U���t�n�\�ј���8OGgﰣ�݁+�N8{{��n7˪HL$�����Բ?�ek�0d��/��;0�ƹU̪��\~���0(ξ�|�1ɭ2���,�r��0�op�����WZ)�O�����Z5�zfhA�a� �:\A���y�@E�"}��Ӭ�R� �~��}M���&`�*�uLK����w�[�֙wr��w�Χ�}�T��mu����w=�����ꦫη�5zR?.��X!�*�&��Z:{�R�e�b�b�����A �XG��<�#0� ٷ#LUka��y�����D���O4� c�Jpk�4��Ì׸%���$&tH0�'0�]�͜����.qKڕ��DEI��SLsg���h���o�t�p���]�;��� ����g�$������z�h�Qh~� 0�� �����oݳ�����|��j�q�K�tx9��/�6[	 A��m�M�A"b�^ow�vs�T>=*���ݮe�����g����@>�Į9e@OiAW�;��w�+`j$z>�ؑq9!5ޚOIgV��[�J����U
�*uO՜a<�d��&����X��!nI2��2�W�6^=n/��vS�0��b�vMAڪ2vOOCx��RGg�}�Iq���S�\;Ӭ�/cwMW7�{��k�a�r��@�)�h���G��`K�f��j���2Zϴ�M�?-dӽW�tyd�e3�V�8BG��#�-c�+-��5
�cwTZm
�W)kXAjX���X�?�����u4�ƹ'V�t+�Q���д�h�W�Y�������1f��+���A���"��ϒR��z3�t�;?��D�o?1�;R��FB�Cc��=�)h}܀d����<�!��G1�i�w��Щ���/���u�	7p��c����%㙂	����T�m i.���!i}V7�k���W�l�1��4�5��S-j� 294�ý��8���[w܌�MU���_�ܿ�e`C�_�K	��6�0�����\��z� �s�Gi�E��c6���"�wկ�h������Ij�U�Q������b<���$��'%��9�ێQ�,rr��b��%*S5��il��we���E�I�����BS1	8�C�X�i��ݭ/��m.��C����ẉ�y ���/�O�x�Gn�'m�,��Z	rȍ���n�&��(���fDR2,H�$�Na*3�}�Z�s�N� ��W�w.���t]_�1̏J/�Sg�$�AM:X�0Ȝ)sR=GJrM��p[�e�)�ye��U�ʜ�`U}J�Z=�Kw��GZ�^f둍fÌ���;�҉� 0Á|r�����M�Dk$��J9PP\��)�K``eU�����J,M�Nu��c҆��'�B��J�EKu�"ErOR;�D��~S�V��U*^JD_�#�nҽ��ࣷ����]}�ߓ��˭t&�޼��BǛ��ˁdE�X�rĦ�I�
�էx9�Mr鰉��<IL��#^B���\�P�cs��H�JV����g�,�rn�D1�]&�s�PP��DNS�>�L���H"�v�I6X:'�m �'����� Đ,�7
Զhƽ��ܮ$��2�C{����� 
f]1=d�� ��/����I��3�Y���H�`�av��a�k%�*X� �c��_��{B&P��A�p�:K,�0}�3s��p�AW�قW��05�oj2���53�t,�� �R���=�Uݱ�(�+��0��V������'Qe��C"��{���Ge�Wh4l�gq�}�M�I9bd2�m��n�����Yu_o�2�MZ�c�Y}�L�:8@7jFJ���Pr�"y*w(���V��_ޑ�B����ϸ�V�kW&��T��-n�o�����o��ٳ���<�@��Kp��>PX����������u���������uO�:ܺ��m����Ds%�+�\�愅n�@R����|[%%a5O�r��B	ĵ����b��]5?�T}�*#ڸ]�x��]-��1Z��N���|�=K��0篌���4N�hؘ���W�}���ʽ�a��ur�W�Ow��4�)���)n��8J:XF�Qk�'��3㓪Э�5���k�7W�fP��7���Mh�6��=>��	�M��皋�D��M�]�8�2S�*�g��J��%g�("_�5W{S۵�f�� |_.� ��3H}?jµH��.|( �,�n�yt�[[H����EPz��00¹)4��t��i�"�P$��N��;|ux�]S��ʄK�j�Fc�lJ�]Ӓ�g����{%�L�rOʴܓ��{���{R�?����n6���{�K�yJai��n�_}��Ļq/�H�����l�K8A��js)��D ��,&��iB�}O�򄀫���ժZ���>��}<mW�>.ˆ
3�2$_N��8�0<�Ŏ{h�y���J�װ'?��D��b���u^��;Q���8�+B�Z��n�	Q����%��-,8DQw$T���{E.�*`� ��΢ʂ3�΍=�*`\2�d0eOhg��F�-c=J��,C�#��X3�М�N��� \8��h{'|��0�t'K�r`��,��8��{YC��Ҳ�=��`w�����6m�9'����riE�Uq�u-ԡѳ���6H�J��4��}7z�r�ǫ �b�}���yR�Cx ^��Ŷ���/���8�)��9�4��pnR"e�v�ә��/��f2���D�<�������}/]˷�Ɋ�5�ܻ�%�XSJ=j���[P�jQ�4S�}7Z�rL�W��]�S�y���ɉ��1��\��>��J'�uЉK��p�ꢓ��d|�e8בkB��$u�&�@¹�������?�{k>;���C���XF���͙�*>U�V�55:HCR���Z~�+P�\��Ir0ݐ�Vv��b��9߻u>D�O�J�y M��8? 4�Y�Dx��8�t��xd�#���nߧ^̀8!�a�)�����ms芫�*�>�u'�p���$����1y�C�K��9�w$LĲ�A�g�����㰎4�G�6�
Cnk�ռ�9'T���8��Lǜ�M ;pP��<�Ϧj]�G4 ���3y�s��%���L�� �r� h�qն7�f#>f>P�S�|�*�l�.��i�R�+���Ô �;8�OA+�����=��ؒ��k�-��GW>��3��Í��7���5�_5g����p�kN���O��v���+��߾wW� ��ҥ���]X,>��;Hħ
U���o2�4"of��v~���Bi2���c���c�tI,��p���6����Y��'�-ǎ�!�IS�bC�Ɂ�j`���������;oW?]y�����b&Lp�'h`_՚üƦ�g&=7nҖ+oD����&�,/
2m�ex�G��o׈���N51�f7�d8\`���TI����)B�&�	���h@��� |̍��!�Ԭ ��a�[�E�����ݹ�o�U�3�u���V�N� �9؆�nS�3��ݙ�=��k��PRa)�^]\��т���(u�������V��#L�(�/�|����^�Q��	�;�z�g޸��֤�{����5��Ĕ�}(@��2.��E�l#T�¿u�r�B����n߯vU�>�$�c�Y6���Q�Q��6}�����YS���s��)��[��ҡ��P��f���ZWl�3�(�f�=b��sӔ�%S�ur�!���84ÌC�׊�qml�p�FeD�l�g��}*��K�Ä�Ι`���{���/t�3�?�R��i �a
�$��Y��iiO�L�N�-�'�>��� ��ʋk�X�L�����*�y��@�U^ig���f֡���f�N���N��:cN���}n�����p����\x��������ݴ�U�Pr8#Y��_$ś�B-:l#��_�+c8�����Ӡ9FL�k:�y/�W�R�m0.Q� :n��Vq�v'��bJYvJJSݔ�,��?�=�tȟ�t3bI��.�EQ��5M%�M�#p�����-x�p���;��)�UOõ��*��%ܚL[����	Dx���6] ��5�Q��߯W���I�ڏݹ>���s?����:��u�z��L��;3��3`6��-��u���r��)�;[�	��FO੦)8Kӳ�;�2��gE��Z��eN˜9]�J??]�M>��Q3����1D}$=�zg_r2zW3���;&�:L��.��.D�&b8z�Zr���[�[/���#�hN1~���|B�_6kX��.��^8o�ٯn�!��zC8�Xl9_����U�@�ݸH�9���>{`�������X���A'�
 �B3=6�f����5dkh��҂�u�����S������ʔx��g��6m�A�֘�EA���-2�����]�$�Eʌ�½J�o-��Mz<��)�)Z���
5�z�9���=�K�u������q��W�2ٺ |��:,&0nH������{���7|(W�S�?��t�ϽL�g����
4|=^��N��&�o;�τ�X\��ǭk���i[-����J�D���hX�8�>�9��q��m�t����Փʸ�a[In}��]R�%�,U;�������1v���L��z���1�dڇ�%g>0��Y�
O�dX)@v]*���o׫-)b=xX㢹��1��]��]�h���j��	���,
���v�I9�p�N�����/��!�7������/�����z�T���z�[�~L8K�pk��	�i0^�8
c�~�-�����eF��Lm���C�ON�&��n���U�͑���h���E��}�/D�&SW{����L�)� �����w�
�~�9\o>l�K�+'��ی���Wco��m֟�`O�h�l��&���ɮ�C`�\Cf�O�����w�R�C����x�~�_���~b�-��}�)6	��?xnu�Inۺ[w��}�)��5N�������v�\��{�����v��`�&�Bp�h���b�+�m{�=^`�6���z����ه�f�D��]�t�#��z��7�xH?L��a�Pm�����q����`�c�T_��tCY	Cx��UB��b�YyV[[7������f~?o7~�.#VP�c��(����:��g�Z�ݖg	bӁ�(7`�do.���n��.�fV�f�j��P�i�E*��wz��jh�����T�M�P(��IX�a��[��B��}`X�|�*��@��FeV�m�:<���de��p��ڐ�H`zI��:����"]�֞$�]:g'��Տ|dF:���0��3�^'^��^���,��z�U�J0�(��Q��t]
���ߦdH5�0��Fgr&:��ξ��P+�H��q���9���������Z��l�[R��R��w��t���4�I�~�퀻���Fz�K�� F�v��*+�?'�]��`� 9�f�-Sͯur����+3�(�輘0�&Ɠ0 �	I%��^o������U44�+��n��������޾�t� �������5���-���
!T~GR]8�EK�"��A$\ڽ�C�v���+�%I���^�`��en�6H��W���mՏ����c��۹U��C�M2!+H�=Ng��Z��l�	V,�4	�jj��G#֊�/_��g��՘#���;�"r��&K7�$(C�B�3���gL��a��Iϱ�����pa'}e"��d�p�{~�v���}^I���a#�TpVNyc����q�8�-,p"�[i�y07�gn��>frCcҧfK1���6��Z��gh��b����)��nL�uJ�i��Q˅<�z��@x&�zF��L��*R��8�,|Bl1P�� ��S���c��X"�A"H[�({�9Ɋ�n�1u���p;49
$W]��z�)@���|���>��'�=��$�w�wDD�����U+o�
endstream
endobj
64 0 obj <<
/Length 6894      
/Filter /FlateDecode
>>
stream
xڽ<�r�F���
�Y%"}A�O탭�q��Z���:���DLH@��h�~��s76eǳ����s�4(������=_�<��U"i�%I���[�xa��d�7�Ň�Uq8䫵2bGRE��7?��a��̍�N���|���,�&��=N���PI'~�5�ba���$���BG��h��A����\�a\�R��h�d?q��](8O��-d�Aq�-�*����f�7jFC���0ֈ��td���/�4��ɑ`2��E% 7�1� �Y�F=����tv k'R��с�\�(5��hД�kg��)��(�ϻf�w't��k���ÿ�R�,Vk�Ͳ*�-����M��f�O�ǃrz�h�}?�h��Ub`ˮ���DUE�BR3�&�����;���-��.F��3@mk�e���&#e�r~��F��i7�f "`�+�<Y���}dz��=��d�*�@i�`�MH��f�;�j�\�.�]�Rf��j�.�t���b�Z��gx:>a��{7Yv~�I��edg"k*R�?Od)�X2��l
%i�b4�oA�IA`��R��!4�ZS�;Ђ,����i��W��TL��@Gs7��4�R"Ҹ����Sߑ
-[z�>ҳ횼��u@��@�.�f���qӕuER�g�x��"r��$�p��J�i���}�([��A�����/��Q�����͖�TK����&r���d���kV�@����I�'1*ʄ�Z> /���O[>�լgɫ�1��b����z9P�`�	�~��*���<�-h����e��PV�������PT�H�)�~J,hV�.��jii����d
�F�,@;0I~�3�X�WR�e�n�jS�k��R~����]P��H�cm)¼'��AMMԩ��DB�~@r�aEP��q<גL���o�a����Q�(��
psFG�i��V��X屜��nJf��<wl3�Y�g$}Y�g���;��pV��c�LuJ��yڏ�x�$�m`4��h��P$��(O���}�L�NN��o�,���1�P7���l�>�]q��'nC��n���2<���<����T�M��1=�A��F\����|]W��� %�eȼV/����˺���8��鋖�?H}A�2��&�~��t4hzț���yC���?����6������=yŔ�]#M��X����˦�;������P����1(y�Sm�$10n"b�9��U��hl:)�r� ���Z�wES�B4��h�[ɥ��/p8�{jB�ശ8� �}N�xʱ-�CU���ͮ����r�_��D)��}�)�Hԡ�
�(���)�<�*�9���ߐиv��]]�w�H�( t[���#�:�AsWlvU�A�ȵ7EޱnAz��Q;^]�� �!�! Ҳ��N�/G�d�cwN���`q�&���԰-�rX�'}*�]}�;����C�:Ūcff,궣@�n���*;�%�%��gr-����"jF�*UL�A��Te�G2]A�v[v$2pZMO�a�:Q�ާi�զn����}�c��}�MI�@�G�Igl4`[Sl�I/���ݱ�x��F4�&��*��S�J<�U���1,'��q��#��z�@��\�6��m+;��x���ص� rڮ�CQ�#i��@�*E�m��rzlvyU����+(7id���h��,e��^����	�dFg����41�T�1d�r�R��b���1��h��?�^@h�'�R�̩�Tw؁�^��A�@���t�/Qعg�
���.�ܓ�#����z@p��������m�R�'����o����!X������v5{j+�G�@�ǎ���VO�0p_�l���1�q�.�u�E �[�±΋:�U������v������lw<ϳVОDXah< ����:g1��H�PT��˥�|�-�E��gR��슃��`����G��4��4X�n�VL,���ƌU�v�v���>�%��w!v��	;���@�zQ3gnp%�(|�e���[�0���Ҳ9�
����Ǎ+Pwb����~P�ZP|A�D��Y�h�)k�x6.�\d,�w:�Ư ao�c�%�=�K�VQ�)���ǢM^���#�)Ȍ�'K��8 :& ��<����#�a�K�Lh���B=#*iĿP���0������7Q�ڦ�X�	\�ŕo���tf�O�%����ԙS��0`���h�����9+��԰�p���߉�V�`��e����KT�𚂧�wƇ2I%�6U��6 ��"!c�}(ɬ�t���F�ʆ;j��aJKmdBA㻻��Hg@�C����������r��m1�:���5��&6)Jfr�97ǎZ��>��{����>cưQ����r�RY���#eM0�����o�#Aմ��}�okha��\*x�:,B �!,��*:�#����(��KC	�b:2�G�ry��]SnPJ�ȣ+б(���5��M4j	���SS:��IXa�#���SZ�s���?����6��ޝ�7жewtHu�����3�,[R�z2F�\>��K>8 ���0�W!4�#��m�S�&����$�^#�ԋ� �@�Tq̪���q=�i���I|vAc=]��,��6�4TP%xʈ�A`]��=�Ƨ��m�n��!���j�|�/�aO���z�c������g-W�	���E%��Wl�o��Q}����[�K�zSﹳ�V�����	��&{.d�N<�X,�J&�Ȑ��M�#KB4��+�wl�Ͷh�P��M�\�W���#�z!;�w��&u�u�5p�X�(�B�8��%8<2�Q̵t��������j�(��F�V�Y����%�
��]�}��!��ZK�q}|���#��9l ���lY����H�)7oWk�����
�(�33�-��_/G��ddm?/1��qq|t���`{G	Dư0��)8a��>�f�EC�~�h9��cg��`�H�Q=�R`(y����vD�ؚӣ��EBDF��[�mpМ�B��5����W�Ra���>���S�Ẁ�^����Ҵ�o��W��{�XC�Kr_z��� &r'�}��ZR�8B5mG=-�'���ng&��S4!x��r��/����2 &��Q��`1��N_:��^��e��#���>"C���^2�8U���^.�^�2F"��%'�(��8�g�d�f�B��rr��p1��3���M�G�Lކ�_��ڙeC� ��DA����3� �!����K�ZG�cH(m�l�xJ�_D���*���]�K�8Tv������,mP��Hw��/�	Yd����|��'�I�q����B�I�qv�������t�
gL�Pz0����2C��Nm���.ء;6�a��D�4��d�e]E>���� 16T&k�,3����FX��ﱉ�����.1���̲=>`~��d����>�m8�m��y�ǐ)�k�D��'��P?6�l3����I:F&>%�Wh�պ+< SI:�>"�m<��\�A�����.4�}@�N7%���ӕ%�s�P�1Lc��%1��jŅc�������@}�uꬫ�Yg7��?y������r�_]�X������'6;�e7�=G����X����m缡"�����yR]L��Ϧ_�oK��O��/OB�q�9�H�T�Ρ/,��¹I���C���"�r
D�ฆ���>�N�Z�2�_��M|����an�ۀ�e�v�qӹ�vP\K�wӜ:�E<	���[[đP���fH|��*�C�
i6g���^�D�ԍI��/����k��Uh%���lj�����9����x���|��o�x@�9��w����P{�F9DEB�'lO}v=N<9�B#חB	q��٫����"@�å^\�ށ;�k�#�	�᜻b}6�Y7�map	�t�����Ue����o~Sns�����)���I�R����k4�Ӯ��H�T�IH��^��v��|:�7T`
��fWl~#��z���:�/цr)�'�1L�ғ%�0�@�`Z���X��F����q�J%���������0h�n�I���$��3���R��2�O&Lш-��M�1�Ԅ�����]��B������{*��T��N~�D�;wl&>��}v
��:�);�)�$;��B�W��4n����t/���}���~fF~8�$J*��p��w�[��Kʪ�@7�U]�'[�B�LGItH6�1L��QL,N�d�`)�(|���)|X���\E䎼�N���{&v�&���E��>˛�$6>��>��^�ږ���O�再c,Ħ�5�#��l�BD��Z�%�����M��G�~z!�\ k�a�CbL㹌��`C��r��25��v���E#�Q��#��2��._�Hi?��dh���[8���*SK.������)�j�;��K�H��3ر�e����G�ڜ�ɬ ��ݧ�GyG�!X�v�	~���̷��'c+�J�P���LR5�R�Q��L@¤g	D�����T �J�����p��+b/i9��"�x� ����A��cP![
��1��L�>�� �/�Ñ5���p��"т�c��c�y%��t�P�����":߲Q�S�<��rE�O��y��n���]���А>O�|�Xa��7*m��xt^�+K�j|��`X���BŞ��dҰ�����O�܅(��?���.D�qd��H�>�C���~z�BONa�L�qx� �������됈�"g3�x@ƣb�"K���������	��4ҙ�Q�yBw$C�x��f�
I���_���IZ��N�y����f^��P_3��E�s�і}��d�t��v����_���F��`X<�c�z�R�P?����#�J)�$�a��)*��t%�N�=�W<%�I�o"2�.�涮���OlĹ����w���-w��J�!��@CA�Q�c�k�P��Ǌ��s�j�LL�bK	��p �/ݠ�SqKwucV<R�1:��q�n��j��Q���k��~O]��F_�����C���c�E!L�Q���*�Q�)��ҁ3O�V3δA�]���]<.�3A��uD���n�R� ���?e�ox����w��nl*�Rd���+�=qx��%� ��B��{Ob#�L�G�Ļ"Ο�*G��a���6���FA;B�!�̐G���#d��/�78Q�H&z"ꓥTr�����e��idc99����@�"M�����T�=6�b�GMf�]�B��N�4�/mc�ϙp�gY�/�FǐD�H2��>�"���(M��C�tϦ<Spuf����8�<�zp��zJGV�ᕝ���Ф�&sW��h�"S���a��^�8Ԍ��X�'����`x��T@k,-X8��k�-�'�ч��1L�)B�E����5 S$OC����i����!~Х�X�8W��G�H��t�V��7��vD�m��>����A�~�v[��d��>��R��CI���'\dgp~����00}�8����$ Nc� �s�C9f_����ieex�0�B�e��O�3cp���g:6�St�c_��/DX����M���B����k�����<N��/䊜>��"�y�n�+4ĹT��xH@��9U+1r�3D�h:d�`�x.*�:�0��ɲ�j=��B���7&������<	^�Ϲ�E�ނ��V�������e��e�G����R���W�OL`;!�E��j ǵ�-�ַ�j�?QH�^y2lU������wWaS7S�Ϣ2�O��Gb�Ӟ@�ر��;�?1�8�Y�y�^���`���|K�jJ%s�f�3>�I"5\8zگ���҉_�u+�n~C�d<�S�Ez�F��u���R� �<�k�� ˠ�˲D?�J�]d(��q?��8޺ �����g����Pv�+�~�9�0�X@&x��ݔ�qb��t�#/;�^f��4p�2�O���w�?��/�s8%@�N�����G�_G(��P���"��M!A����5gIEgzp0�����~�������1�B-����1�KIA��j9g>Q���=3�Գ�sPi|F�j�r�0�ir66�Q���}��@ߦy+��ˮ��_�|/h�i [�)�6�Q,�~D
)X��wW������(�;��H<�'�M�� Т<�E��la'�[w�D-��eM��2d�$@��O)��h�UG6��	���/<�2���S����z����}�w�������2S��)��,)~��ɘ��|h��j6�D��-5f6�Rg��5}BǤz����fG�d�݃9�
=�V̍k�^Cs����m��_���7A�H�s` +9C����Qx�ʀ�_}Ne�lE���9�<��:΅���t_R��u�:KG���~pk�X>m�_F�� F��C�@˕�IG@#}l �>�������	^��>�EV<��rq�>A��'��b���W��=uE�a,F����P�3T"-��NϹ_���-�)i�'���.���ٔ���Oo�)ok�/�y��4�<@��`�N��J�b�F81�d�
_�(M�-��D�ʔ8��J�\g-�&�-K�$��'L�B������i�3����Cb"d�91q��/C����+3h�Aa ��3�S8��Ρ#:�ȉK�`,Y�;�!x�Oϡ�@�h˓HL�#q�G"b��\�#B:����fB5�hؓ	�d;zx�y%,<�z1�d��?�0��$,�|���q��w7����x�
endstream
endobj
91 0 obj <<
/Length1 1399
/Length2 6028
/Length3 0
/Length 6975      
/Filter /FlateDecode
>>
stream
xڍTTT�AJA$�� RJ�]� H	�03� 3��-% �!�-�HwI�H��"(�������Z�Y��ٽ���wؘu�y�(+�

����I �Z
�" $�	���1�?jb6C��BJ�7E4������
	��8 �� �����@�9�����xu�L̦�r�@�ml1�2��.�_\\��W8 ����0�clalE��GA�0�ǿRpJ�b0�||nnn�`�3/
m#����1��}�3�
�?�`�72^b6����[���Ƹ��0 �p�C`Hgl�
C�‾�&��C�v����������w�?�?�����
�Fz��6�5��h�b�1� 	��vpFa���`��
��s0�"��� ��s���g^g��O�|?�`����*��L��?%8��݃��f�(7�����Z�uq�{��;��Ԕ��`U���l`@��0' ������������S�E���r�� `>pk�����
0h���7�["���p�������dǪaֿe���pw�!{{� ����7s�yAQH���O񁺆���߈��)(��/!q�G@��WQ@TX��w����_Z]0�Os�2�!�Q��o�������Yp����
�(�-� �N�$�`��o�
����g������!�_f�_��a#������B�%�?]�`����r���MƒCi�����*pwT�������v�M� G�tQ������n{�����_&�M�.�����?i' ,��h�1v�XI����
s�u� /��� Xp>�5
M�s�� 1�����/
���U�Fc����=�%��:���N� �AvUAM�+���xև�'�֍��x�f��.'��\��/����ސ-�*s��1�{m��!m��k<�>���?��H<3z�{$o[�������@�����a�=�k�Vu�,'1R���n����]E�!S�z�E4����D>xj��-[�U�$5>����������G�#?��c���D
�{���:��\,1pn��McJ̀{H>8���1I�ִWaAXM����<>7�}Y���60�5&� x5|��G��vD�|���s��s�g�'�#� �Z�Ύ
�n��0�|k
�e^pw���o��s�#�����y���Z�ɵS�]r�6�B�"��r~�j��I��@�θ��6��nV
SU�s3�ᱎ�[���o��2�]H25�*�v�zÉb�z�z��-D�H�1d������Q#t��&�C�!��ƃԁP��W垾B���#u�Ⱥ��w����*�8��%�������?:�֙� �ܑ?�~\�Oa��'��5��oոxe.^�c�nM��J�4��������E�y�w.}���'��[}{�&x5ʮ��2W���=Wo�X=@9$�x�0�T�F�N�q҆���ڠ���kvB�"����8�R����l2V~��%�g�>:񒡳�i�%�.�N��^��B�-���<��˩��{yJB��xw��M���>⨳.���߻4�.H^0|gS���k�����dDN'F�B�@�����^"S'dI������ u�/�vN�D�)����Rw�ˡ�l5��w�e%۾�e��,��n?���cr��'����eY����ͻ�F��/�גp���=]�J'�i���?]Ӓ��O�w)><�a�'�㒒60�*p4bɠ�/��!�YAƥ��g:ť�ʣ�*�덮7���Ĭ��Yb@�ߑM�=[1j�Tzи�lLO�0҈x-M���Gp#��b�>Z�ʭ�)4��]K
��ƅ%�Q��b�fpQ�}�oi��Y�����^!_�"O��(D�AH�f��?�ޡ�m��H�Lǩ������pB���/^���7�Ȫ�]So�%Yc�����3�}�d9�Dy�_<��a"8��*���M�{z6k{I�����z>�O�.������_����h.��V����s�R�
��5K�@L�sq��ɚ�srB��g��;l�6�W=c��E_�}��cf���:2�T�]��fI��Z�B�+���}^y�EI�F���/�P��Jn�[��Tsw�ѩ?�G���>2�����`VrU�����S����p;¼5�`z��V����d�͎@�׷4����Ff7�/��kw=��e{���$��K����9��2L��k�!݌Y���t��$�����G&NH!����S��O��7�hp}�|wC�1�m�SuJk;���AR��໺<f��D��bJ�8�N�8��Đн/KI���K[UI��4���������KɡZ;�OF720�&��N-��7+7 bm�j'������l�h�k̗�F�>�_3�7CžfM��w���|_G
��U��W���/�R�+9�]��-C߄*k�z~}��/�\�J�-�ܾ���DR��^�� ��D��#����[	�۰����؉����E7ۇ8�4��^}���<΋�<*m���wHOvY�Db>nԄU��%T���f��>52yR�T��U;�p�V�9��&]��s�h0�*���}����������;�,xHV��F}��V~�2#ǉh��b��t�U�H'`���[�r��g=�C�:U��a�1�K	�XF?�Z���2pF(�������bNT�Yj����/;c���6B����+h�<�U9y3bP�yj^�c2f���5Ӆ!b<���bO��o�.!JN�BՁ�,M��,��%P�5��2�1��n>�9{�N%��{�G�T´Ԯ6u��n������|���`K��p�n���dV��P���EkVM˜��cW��!��v&��J�L�ͦ��2��`�+����υ��vcH�~��ёɰȮr�wJ����3C<� ����'�u��j�W�ۃV�km-��?�b$��8v�S���׮��*���fL�Ϣg��z�"�/��f/��`m&9�'lOYUU��F�������X�y�w/ׂ�o�I%�ߖr�eR�$vk��݃8q�G��XUr횤�UJ%���֤�jZ*�L�������\.<�q7}MQ��x�m��b@�X-�Hi�(cT�S�y��in�u�wR'<į���
���fM=�#�N�2��)DmA���~pI"����{I�o�x�J����N�Q�Dg�F� mJ?��!?��<�7!�V���Ax(NO,"�����;E�2���[�"M{.b6�RM�B&�Z���]5Z�'�^F�n��ꡠc���(�''^�E��C��2TЉC���CflBڇ8	9h���M��
C
��u)�Z�9��T��ń=�r�)c�3@y�3�G�Όeی�x�"�C�Z��RiJ���>`��U�����(�x�O���8gq��JS2^��/5JEx�P3��7��j��Z�����_Kj07S��j��:V�|j��)2�ä���A����+�y����W��Ɋ�3�!����)f8S�5��km\q�֏�&���聊�]~��'�Vqg'�nV1���{� :·����e�A~�m)��[2?Fu[R����=|�B�VC�$��3T]�\`mr�0��:EO��(Ke�e>mrgVL��b�ʗ�Ԋ_�y�X7~ǋkK��qN\x����J�J��>NM���Z<t�+>'��WJ����j���F�A���O9�wF�/ChC8�!ڴ�w�.�=�7>U�MZ&{����־���%׭.����3!z3HVDȍ$0��;#�ð��X]��`*#lk���y�l�)g+hV,�kΕ{v�ƫ�/�bUM����\m���bp���8s�hg�ѕ�:�I�����[�n�_H8G���+� q|=��/�*>n3����Y�N�Z� �Q����f�s֟�V���-��n��y~>/{K:��\m_
�V���&A�F�0��+���ȱ�wʯ�޸�Y��f��QF�O�*�� l�L��\��W�����8��t}J
n�b5�)v��͹ȵ��:��_\��5�h������
�V<˪���~��cc�b�J[�g�}*��s���|E
Yjj��o�ǯd8O�+SۅY1�U��iJ#�o>h�m^�P�9?.��L���Sg�1��-��k�k.R��f�Dp1nfw:)�Ej��U\���	�ڛ_zΉKBm8��ԭ�{ ���ȯx�J�KV8�mt��^OɈj�� ��Qꎊ��FFpn	S��to�M��o�r�O	�u��'--�Y�Q"� ��E��\f|构"&���E�z�����Իw��,b����9�;��.�\�j�5w��o��ٷ��r��R&Y�
�~X��	�f��z���j](SQ%�b����8��y�i��OTC���ǽ�Y��zͩ�o�[�=�O�|>�跗�}AXڒ�s�~'��$@ơė�+m�I��Xk!�N��YPIu�/;Bw;��;fv�؀6W�g��+�����q���l'�Ņ_�>���V���k����#8�GJ�R �B���).WF��fQe��I�Hށ���+�t5��"q'�^��>��z���	EBLֻ|��%�Q"G*��|���ыl�TQ�q�p�t��ư<��o[pUc
�p����T����x��\��� w��dĞq��덈�;������z
�8�Q��/-��P8G�o�(^�H���԰��Cˌ���Kٛ��G\��9N�7&w���R�{��?����|��6�r��� a1��aP�^(I9-��^��7�5����3�䇒N_��mN��#�c�8�5[�פ�	ȣ�#����|��W,[��%2�+e�h��uj�X p�E|�?d�Ź�Fw���btq��.lx.�\n���!��xZ�1���V3��:��EE}�:����钹������ۈ�.>\8-�G�Iߜ�%�4o���KP~�w�4^?������2��2�B!'���d�n���=Ǒ��&�|���8�W���4�̾D[>����Q��׼X���0M�[�����<�3kΞ�R|H�J�������i���h�b�jX�
�P��5y7��^�G�&�Q��m�|�H�-:J/����!D9(�n��X~{����HUwB��㰂HI��jZP#����6�N�QI4n���h�j�'w���l��y�`~h��1چ�=��O��������q���Ξ<s�ѧZ/���W�dV۱��|�)�{Fzy� ����YkP���륑~��"q���M�u������L���ס]�F��)/>
VD�s��R�S���LxeK�7�ח�OE�3� 9�K�v	hVd�"aj���C,�G5��H��Rȸn�YW���O�uM�	��3�	��8Hf�L=�Ҩ�>|60�_�|��3W��|v$9�ѿ�y07Tm��o����N&JG�1S2��D����sρ�ލ%�Ai�����������/�ػ훩��Z�}6��g��9!�;`�[ ���\36�������;�=�9_�B�6��^�c�6��~�����+��zΜ�H�Lv��s,�p{�-��m��3&������T*њ�^{���>2�����B`:�0��̪-��զ�,�6��Ѫ�J������l�l�OZl�C�>�8�Z}��9[׹}��������B������E���Y��bR����������O�0ė^ t��;��{��j:��*�����5�<�%�Ӹ�8�Z5��	�獆tsZ���d�J���2�*�x���.�b熼�}����� ���S��z��}�d��[?sv鸳2��G��螗,�V���d4�ީ���[��;�c~�U��\c�}34�1���,s�-OՎ
nuw]펝�/M%;�e��(脇.S�����®����d��R$�*����A*����x�qGQ�9ݫ�^�|$��@��ά����Ľ�$�>b|����<}���J�g��.��O�
�D:%Mم��S�B[�\u�����1pHKFO^D�Y�T`�߻9�_ü���OQ��S��7$$��3�r�$��26������#��P������;�9��)�x�Gxp_��7��nm��y�g�z�hݺ���<��z"���Y\˲\�BU�
�#ӫ#��˳	�K��ܲ)��?,8J����s���G^���?j-=N|��e-}Df���N�&�T��g�	�sZ;��X���1����
����o�S4����J�"�Nz����7v���^K�0S��f�ni"�>/>n�h؊T�yn��[����=%f�B�X�n�� v=��ыN5��os���z2ޤ.������%g�z0|����Es8��o�{k��kf�uK���P¦�-�ݥNI�q�J���3�ψ{�We���iO�[.�L���B1d��f-/��M��Q��'�z�! 5;�Ǘ~l���OS@ �T��U��O*C��F����7�'�1����nq�q�_?s��\;��y���+˄�m�����̱>�W�S���a����rT������>9�ߨX%gxf#\�����yE%tɤ&ì��n��&d�Ąh�P?e��qjM�*M@D���| e��ԢI��I/`�r�T-���d�J,�"%��2rIx`�hS�pC7����B�H�>|)_�@��C�|��KU�nw�&��ہ�M�|Z	Ҁ��v�����YZ8O��B�/Қ�H�4����-������q}N:r��
`_>��͔j�9��D5u���Q `�7
G;d�9p�{�c6P�p������9xʔ��O�����c���5�ЌǴ���ۤ��q>IL�m�I���'ϕ
�������|��}>��&�6���/�]��%E	o�O�9d������x�RԣR[p�L��=�b%&-��ib)候B���T�'8QQ� m��K���������Lc�{�u��`Ndɋ��*�Sr�$D��3L�o%~�2mN�1�T��_����
endstream
endobj
93 0 obj <<
/Length1 1413
/Length2 6276
/Length3 0
/Length 7232      
/Filter /FlateDecode
>>
stream
xڍwT�k�.��tJ���H#)�5230tæK)���$E@ZRR@J@@J������w�Z�Yk����������>�<���D��	��$�Z
& H�$��0��áԄFP����_�((��S�������  ��� � ��ߎH�$@	�	� ���Hԍ�C�⃂�;����}p�r$$�x�䝡(�-��;@��m�p�>�u��W
.iwwI ��ˋ��ƏD��r��`� =��	� ~h������r `n���H;w/0

@+�0[(�၀@Q tq���&����Y�/�Oo ��I�'�g"�W0����F��� ;
x������� # ?�p7$:�	������n����h��٢`.�n�n0�O���i�]VF@���P�����)�PP[t�}��'�@z!��v0��'���s���)�qA����C�" HLT u@�m�?���@~���\�. ;4h ���!�s{B�(h���6�[" @`���P{���h5�>
�0�wO  ������^$��������t��y~#��MA����	�� " 1�!��i�Ӏ�����a.�'��	���ݼ�qx�Y�?�����6��P �?�o٢���	�+����?���V��/����2s���f�3������hZh!��@���1�7��p�����hr�#���i"�M�����m~o��3@'��P����' ��M7['���=�_&(�M�.���EB~�NPD F��>����%�� ������ �Hwt . `�D����8 ��S�K �E�?�� ��K�WQ[
��_K�����/�C��P[��i��T�㫰7g5�^|�Gd&8>�s��͢�<.H�S����ZB�ʧ�{K����u"7���o�u=~d�3��K�+�zc�[	gFi�>����20��m��p�7
q�~�١Α��!N�S@y�գ�]�[6?1�Yw�ZT��l�/�0�"��$G���)��x�|L��Sx�N��NP�~�aQ�C�/���lY�����b���[��;ftL�'Cc�~
[���J��#b�혬��d�H��µ���e�?����b�XF�6�`��ʡ��\?���"�N����BB7���4��/�Ed+c^<�y>�K�q(� ��>��J7۟O#�7`e_VG�pITd�*�~,��`(�cn!2��+e���l�t��us�7�VR���B�tYs���E�I�9��0�/>`)�Vۘ8��bxV.o�S̊��;q��8R,]��2Uw0�;�^�K���u���?*��;�y���Ǵ;p0��9�uB;#M��22�ܽ%#��q{�P�Z��~^���t��Sa��'s�5�Q+�Yq�?��S�UR���������ئ&�J�;�ƫ�|��w��q+bk�`�z<ٟ��'֚Ҝ�K�&�\s��b���Y����-a��^�'�ը ���X�x���<p��?�A�N�l��FR;v����(ّ��{`��⚔�1"7�>�X��w^��NDyq����F<���24>��ɧ{`�o���=�\_[�=`�-�&��1K��ލµ���.N���Ɩ�"K8��!p]�ŵ4#�Q*�W�A�|�u���H����ǎ��t�	�����]⛢���A�TfaK��x���D�E$�<�y�Ό�ۣ/���T9�zL�V+/5	��% [U��|��8�����e5����^wwd�y/HM_IJ�b�[����`��}������T���C�D}�R�,�1:���+����A�fU�0��ZP+�xj��.V��c�o��"��ق�gÔ��Sӓ�37>(<��]� Y�Q|����B�\1�H�-L����hvO�)�5��r"�}��Ɣ#j��s�a�O5s�67��[E/%^�cg��e�Vd�Y|㴨y>&6'S^h�JOh�P8hC�o��I�󗗔8C��W��Ck�J��n�r���Z�A�^8��r@���N(��\97��:Ҧg�ȿ��� I�;�n�vK	y:|�of�w���� �x���sްL�Q|����Z^,��iuޥ�:�^����>c|^]1(�����.J-cC�4H)S�c�O2�e�����9�k�#�A��j@j��v�V�'�s�������_ƿ	*��qT�(,��q4q�B>�����8��X�!;uP��"�r�RfT?I�p6J��*�o�0��	�T��0O���=ߢ�D���8U9��F� �*fމ�dU��ԋG⪨�Ö��P�$�ed��T2ް�A��v���wp���|�4�����i?q��(�R�q��6��I~%���Z�XWwk�������EÊ6,v.�����$yQ�0���/����F�\CW��h�0z\|./��[X�4����j�I�TC⫹&�x���*�[◊Ibp�8�߳�"�|�}�
zPq����)`�Q̔���;�ٝ���u����S�����irIeޛ��z�?��9�p��Ha�toV��TN|ے)yea�Y!$�~���ix�R��4�f�ㆷ��}���8|~���ϋܽ��l�SH�K�3�~f�ڴ�}��A���{�J��S���H`l����	�ҝ�R��s#���9���Sf���M\H���F������A_Y��,���<�c�.r�NG�ȅ�."�Ǳ��"�穜�I*�=:�VM1b*H��hO�O���X��ۋ����$bԬI�9��"�Mj9�Ì-�-��� )�Y���"�c'��d�6;��!��S�Q�l��}h��Ut  �̱�����î�>K��^e+��=୳;\�k�IϪ�4�׈
"�������V���������X������4�ڎ�70JګS����6Yz#gJ�������#�MJ�b��z��V�l��������+��u`�8[)��H��[/Z(�;SA�6C���׆�lj�Rp����q[H�q�2�`��t�X��5P��Ǜ��L���dYC��J�j$D��wP^�zi#���ch
��5
��������ۇWܢŴr����8Mk��m���v��а�Qp�z>r�5�	C��c�xd<'�.�kǴ�c�^�/3&�$��ڬS�a9˧|n����ڲ;�4l��µ؈�ę]j�s�o����7��2�����Z�|dK�x��.3i��c'x�j����7�r�ˏK'Np�A�К���$�^>i��vo<^��q=ᰞVD�YL��]�d-��y�ie�L�A��	�������f(Mqԛ5���j�E�����Is���|�
��7��B����i��#:C����A�umv#�w���Ch�)B��E$8}b�à�V���_70L^����,r���tJZN�i4��'U� ���K��M_�S~�����7�Qf�x		���p�&�2`����Wq���1����w��]dm^�n�r�L����3��5�ƭi���(��B2���@�ι1І�z�D�,����H!����a�t@Ϝ�+�_�z��^M��x�e�Z�*�𯀥�6�|o" �������lpeI^�d�p�_}����\��{SMpf��~f�_X��5�D}s�LQ��<��[�tJ`s�<��l�(�slxvS�����%��r4C�bEAx*3�w#ŭ[:������z|u'lD�W5�vi�ih`�r�ͫ�U[��ׁ�ެ�~�C�j��M}C����ⴣ��T/�$=ƎYuE3Ě傢��?};"�M�h�l�2&�����X���`���A����aY��b��1ݤ!���Ӷ]�m�����ƞa�/#PeKa��W]ν�u{|c��0�$�R�6>� ��;ς�����$n&
b��2�^>�>Ki�q�k��aVڃ����N��5X�PiQ�ܻЗ_ v4<c��/�iCt�����;��Z�6�{���d�/����ٙ� ���}�b
]M�q)�r�+s�H�*K�'i��O����	���V�U�����D[Py�bQ��ܐ����'E��]�ޕǒʸ�~���y,\���i{�Zk�i�b��_�c�jq���O��k.��&B>Sb0��$N�(	V�$UY�%�H��ފ�v{�x��+M��qk���咡M��=c��{M�eb�a\6��s���J�ɣ��3d�i�u�/���`�@�q�q��٩i;9��� ��q�M�R�5\g��B;��[Pb��ʽ�6��!?�,�K�w���l�����N�r�����=w���ϩs��Dn��#�IvV1R�*D��9��y�tO�G�<���jɩ�r��L�����#K��s�\��f~��H��ޒ�U�<k&;s$������ˌ�K�=���� Y�_�ۤ�e:!�Q�bQ����S�R(N�e#�a#/3k��� ��Sp^ܒĝ����5��3ï`2�Z;;��a�}�e��`��/ጒpbBK��Ǻ+<����Ƕj�s�{��8jB
��Z�-�ڳ$\R�t��:�YMb��Ya-���yhZ2o�)�y�-84�H�~���"�?�vʨ�%��]��x�ͽ~�0S��.~D³ma���{FC'��N�K�����ҷ٭z}Cc]��s���/C�<]z����Ƙ�F{�<�y��F�p
K�	��ܶ�7�[�a<cU�^�v�D�!��_�P;	����/�ݦm $=�u7�e�:��\����K�����I�wuk5�^�D'`rM5���8��&" 滨���H��-�r�.�r�*(0C)9����y-�a^�'NJ�klwc@c�*��qK�y���tRo�"��_A@��/�㜍9\/B�׫��U|�ɞ�qg��M S��l�sC�=�G&o�W�Y幠V^��Л��y��V����s�����M�H��<s(i��p�r�CK�	�,��]���F6�5(�~�>4�"S.�/�X��X���F��ζl.>E�U�1*v�w��
�lnt�����}���b�di`?��?�4JߋO�,`��+�W�6�m�t	�3V�G�\2��I+����%%�6���̢��<d
�l��C~Үs,?F����Z��[5b��5��B�*�����D�g����.����6�A
K"�6����co�c�脾-�?���gǌ� kD�<_�S%R��ū�	���!Ȃ}LlQ;�I��;-N�r�����+�o7b`���R;�e Jj��j��l*f��_�u��&f����C��[3�j��֛?Y[��f���\Z�k��X�Uz㛋�~�R�k�i������ܥ6ͧ�;,�ŵ��%���f�eo�[v%(��p������u�C0��h�E��3��Q|�h�~k����Ņ�>�^�����Zi��z��7����_ߛ.q��ܾf�d�o��?,U�(���(�����8��{
�r�#->��B�ݓOo�iՎ٤��h?P�R���d
�a�3����H
Ĕة�,��ڛ78cU֠���r|�o��sP<R3��仺�f��ə���+&&�g�\t]����３��T�F��{#p>�*.<�PH��0ܶ
5Ħ"Z{�5��!M&�Zq��I�!����S�86%��{�.e���Z��E	@�ń�2��*�h:������ľ��Z�Ǵ���(�N��_��4�DHE>�9��U�� �T��Kc4ؒ�U�=�����Ϝ�=#��x����rG�L����kz>�3�B�أ�Ƣ�v�r��	ܡI����|��y��#'�n؏�.O�{Sa�Y���<�uB�����M����Y�z~���g7�X=�����<�E=q�+����E� v	��H�k"�p#3��B�֥�IpA�<W����6*�)yȲ>��Nk��擝�N���-�+�7���\_� ���|���9<�}k��$�l���Y+
/�Ml��o3W椺���Q�[�g81t���=Ӡ.W�i�ƣe4���u�\��e%��S{`�}j��,���xxR^�����@r���F2S�<܋v|����!����fYs��A��]�^��!�����º�h�N�jЏ2K�.1:�M�O�B�y}^5~ݬ)�J�&1��x܊M��� î�A����k}X"Ip�1�oqtZ�a��=vK���^���h����A�+�1�?i�L;l�\��4y��������n9�G�)�}�ԏ��`�z?Z/���P�yRchn�L�ޫ�{�E�P돴�V�w Q���K�UVQx�33p�ߕy��df)�-Zÿ�B�XO��D���"��vX�s#i���I�ة���W�+�����lŚ̆�����g����+!�)E�+�I:瘙��^��[(�Q)㎃l�4���f��lކB�36��)ah��ǔA�Q����N^��Q�ȃv�s:�Ԃ��wc����Av���Վ�����}���pɕ^%�u��0�+�ȋN���άS�Zq+O�-9��+��LhY�B��n8�-�{?xd��I�q$5g\���q&K)=e�5pq8>X��z�W�f
г^�8���Hɒ�+ޏ�bζ+e�ML�Rq�\m����ԝ)8�<Ir�Ļ�V(%c�F�9��gW���*�3o����72փ��8�ڈde߹���Aƾ״��nOL��P����=�Pg�m���_n�l�4���b�V���-�'�k֗o]��ˑU��I������p���G}�� eI�sW�eյ�ˊ��#�B,���v�d|�0�?$<M��u&�;Zt��=e��{�6��1�t�3G8�Q?���_�+R����[�&f�y]l|�����U`�:�f���W�ꧠ7{���mj&��f���>�we\~zOC1��oU���Dr���ѷFŇ6�}�c����o�a���Qlh�n�v���M{G=ҳբ�����x$�����қ=���Ĳw������+��riA�W��=I����=*\9�O�/�a���Wĸ����ZZM���n�ޖh������'�ut���q���Z���|<�ƕ=))z4�Q���E&'7qE�3ߵN ��ZC���.gˤ����fS�}(�j��-�`§��0)�=������
endstream
endobj
95 0 obj <<
/Length1 1450
/Length2 6356
/Length3 0
/Length 7338      
/Filter /FlateDecode
>>
stream
xڍu4�k׶.Z$�.&:Q��-z�-j0f��03�]BtQ#"��E'5�w��{Ht��s�{��_��ֳ�<Ͻ�����}_��;z�|�P�-L���	�%��ʦ  P����1ΰ?fb��0�DH���"
�\ٔ��+?m$����J
�I� (�#%	P?�C�� $�&fWD�z�����m��pA��b����.0F ����Վ�3�	��0^�J�%���J
xxx��]��H��=n^�� 0��a��0(�'a����?1;����m7D�a<�(������w�\m0T������~;��� �/�w�?�?���������#�vpg@WE����П�`g4�*�w�^9��P����������4?������4W]VF@�..0M��>%8
�j����uB =>vp��'	����1��SW��re"��f� D� 1Q � �8�Lo��
�
�4_1��qE��H���v�������a~>�	�{E,(��!�-�� �'��f�{}u�(�'�x�=A ����׃+yA�g������������ߌ���� >��$!"���� ~���w�b�˪����OJu� ��U��"���.���7��;� ��p��}K�r�#���_!�?����i��Rqwv�s����w���p�ew��\h#���߮&�߳����]�Uǀ��Ca��w�h�'��@~��S�J�G���h����'�v5q��}uV� ��@�{Ke	�9y Q �{��^�(��K� ~s����C����0 `�C`�0;����'���b����U�����%��yb~!�*�B]�/�\������ �y� �S�H�T�ceH�Q�<���~�0Y����k&K�tW����	����;�y��b�Y�dJt���M�����n·�]����9�@k���5[[�m�P{�:�n��n^���ck��u�?Jv9�l���?�$�"�	i��h.U��mQgC���o%0���v�D�tN�b�R���\52�塞���k�Fؗڔ��?�� �f��/�z!�n��{$ZOq�=�_���7���IM���4{;w�Fۚ�'����͝;�`����_�"T�'.G��&ɦ,�Iw	6Y050�OJ��e��q8q���ȵ� ���~��R]���z��N���<���Y2��3��x
�{%�� ��5�w�F<>���e��f����e��M&�� '#�A,V��2λtb6R��VZ=����ê���=�btN�^�Ě�CO
}�'���X
�a/Ķ�a�G��g�q;n���D�u�AնJ¬�В�I"���L���&_����7�W����f�X����櫹xE�X��"?�������uSD�E�����������?ܘP/�q��7�\ʶ�E�0ɿ�GHo�3��X'�DF��JW&m���\��iz�d��|e�V�:�U�S&�0��H�/OT�b	��j^�Z\��&��XkOk�p'��Z�Ȗޞz�Ύ��y�p�j#��3E���~�\,��`��b�p�KO�bpF�^w�r�R ¶0�zeArn����V�����ǼLҸ��Y#`�F1J�|h������QS�'�Z(!/���0om�¥�������t̴��;~�5W�ܸ����^l���G�t�/1�����`�p���/9�U�)5Q���լR�ka4�w�$�p�k�3M�1e���yuo��ΧG��j�=�E�f���J���}2���
qhh�U�<�����<�y����0�J�C���}Y�P�-��E���ޚ����C�Hd.��Uz��� �&������j����Ͷt�,��kN��O��)�I��>yH$u({<y-�������Ӏ��)B��|�.��梸��!�j���C�^U�ik#�tw���4��m��*���Ɉҟ>v��}���[n+��q{�K#�������
�Sf����K� M�곙�'�J4R��6d��B����A���:��.�/�f�B��p���of��sv�;}���S[�����C�����т���I�Ŝ<	z��D����M]��8*ja;���~;ñ;��E�I�f�{��WGS�4�(F�澢��Ub���7fŖ����ez�0Kʮ��S>)�wl�c{y�Dr{+w��o)�P�����T���V�hўs�x��LG��^Da�2O�:P&�8�:�@����y���#p1�����M}$��i����su�������y�:�	�%�|�̀����&���/o�	�Ti��ಎ��2�y=̏�Y�QE �-Fg?�5�?eF��v��	\���w�Ax�N�(P�4�1	�S_�J��8'W�3n���F6�Lߣf��g��{K��_�E����X�,l&�|�)�"�~�sJ�E�R�[�nWV�J�`'J,���_�p9 �ā
��6���ˤ��c����.G{�xFX���sҼ��̿EE)�s�q�"Gz>�^���o֗����B/JL3b����_��2$;ߪ'�TfѺ-
i�6jA^fM7|���p~N����T����h_WZ:�#����El�F�C���T�e�|�
t��X����㿜Z�`U�������y��wtDi��i:�)�b�O~{H�Z@�Oؙ�'�&XJ��Y�l�#
�f<(s���<Gle��@O��I�=�{\m����i�7�@a:^P��A�C(�z�ۡ��1�6j��_]u-XW�&�c�%+]�b���&t���ݟ�𨈕�^t����R���&BTr4ғ������7��&g�J�^�Y,>_D�w'b���羗�א^�\����I�n;u�$P��1Z��|\�G��ـ�˛���15my������~��=�3���#�2j�j�ʭ�gu Lx9>ޠq��	sQ� �ݎhqo������c��FBFiE�$n<A��7�jқ�r��+�Ԯ�k�T�Zl�#�g�f�϶��7����{�"�1��Q ��p��_������ԏ[�.c��m�j���2*���&��^:	��!��_d�a��4e�=�6+f�3|���KK��C|�%�����Z�gqda�X@��\��B.^��&z���F��l	ك7����	�N��<D&�2%��zs	t��Ȁ6_(�z�-���~-�v�5~�c��ʁe˦7��+x�_׾�Bmc;�B�1(����(s&�����4N��F<U[ZYJٻד'�sn6��8}���{۟:�i3Wq
�����!Nm"Ƈze���ham��J��"���[�f�G]hXn~��>w����UJB�R���r~N]1CG�9r�u3A�]�T�(;j�u3y��]zM_�����'iQϲrK^�3+�:7����m�n�v�c�\1��ĭ���9��o��&�׈nx����d�LO���x��i_��`��k�Q�s.`:lrn�=�w�/���V��`2.��e��kÄ��Fcp�״���G*�K?`,��Ԝ� ]Y�c�H?�D���wѼc�܇��sG�[����&h�����zv��q��X۰f��!4���G���/M8�òF|NAd��<�Y��Y],,��,���Z���+������3#�m�mz������֡�e̐�p��|gd��P/��{
an����#�M�I4�2���A��������Q�Ĭ�� j��x#��o�aG"=��"���BP�V5�����Ūiڐ+���w� L}X���4?$*����b�hw�k�B��Z߂P�=�g�Y��т��D��f#Wz!�&�k�2�*��Ggwsr���M<ng�	����R3�u��ڴ��]���`��	�,����X�u�{B�89�J��w�2C�;L�R�t�7�־�8&?��G���Z��
��6LRy0(��7���!�l|~����NwN�q�h�j��ۚ��C`IC��}�s��gE��\��dߋ�qz1���P/���pUОQ�	��ցl�����1)<ˆ��|v[�.��Tmt^U�[/�`��N��,FP/<����j���џ�d�;B�	��5�Ź;��a�֌_�+n�E,����wjB�#!x�񭇴1��������ݺ�d*o?�nL�g5Tޓ���1�:~?;��?�E��d�Rx&�9�2���Q�:�q�{����A��2zVRę[��"�I`�;��U:����Q����D���P� %���V�;Ƚ��wn;�o5�QxN�p>�ٓ���D���a�p$��܁@j�\|��b�ΒtgJ�\�ޯ����~o6�H�,cT�n�9ڻ�z��@�2"�*�8�ݘ+(���J��7�E�s�*>+������|Ψ�|/�f5��l�6�-��l{�I^�ŰÝ�R����P��J�)-�_�\��|�Ý.�LxJ�6ָ�%�=�S
�T�X"��VO�& S��<����"��$��,=�q}��a�S�-���6���tA�%=��L�o0�ǰ��p�k�X���ԍy"��8���R*;�fk���%%���K���t�i�PZk9G	?==/��o:�z2�ߕ���\<���w��p�,��6+�e'�B8�|�N�}�-co12�`�B1�V3Q�}�p�^�JN�-K1�Iݺ��z�ݿ��.��շI_�e�����>��z�<E�O�k�׮��>�(�8��O�.6"DR�S���|r��s��2�E\>�[EGw��9	��=F�V7q�K�}\�%)G@,&U������Y�0j�YF�)�m_;��!'j�5�������%R�tы�c�8��paB�³'*a��
����֒N����.<2EZo����� ]և�S��Ġ��7�kdgIi��:�
�J�������9)*���?�<��,���v�(���3�xU�Õ)�#I���T�H��}ָ���("��8��k�"#��3�d!��y��)�4/���)����{Vtb���쪟���Ex�e��A�Zu���S�������_xO_&B�����)׋l�ḑV:��Y�|kVzs��˚xEI�<L����aı$�|�[g�l�f=�f/)�;:�/�������}Op���е�^Wsώb!�I]߳�R/L�{=2�wh˽'NP76-�%G�Yh�[<���﯍Z��
$.%�ȼ�VoK c:".�� �BX�^0�W�W��_�����f)�_��MO��
O{h}�x��bZ�>�p��P��	)�䃦�{��[�魂ou�&�?�x+�v�u]C��qP�ɪ�zׂk����ߌ�[�	ï�.�|?:B؍�7��]dz����I�E��?.��s�K1`ɱ&\���$��ig���	�_Ap��}����ݜ�EiN��o�g)|V�����Z�`ǚ�����98��!M+�]٨r����M�#��~��y���m^^G��i�g�g��'}���v�O�،��f�+�*�@C����.��wNM�f�,�lG�*?�˵��t`�6�����`zA'`G4�X���YٞW�)|;��#76�_�8v�c���Y�AH�;V���IP�3�8����E_�Ո���kwX���M]v���0�۷M�<��#�waY4�:&��ZqNj>�d%\�n�/��p�v:���h72�=+��}?��U=B�\�L� �~Mp��ʉ��O/���	����-$����$$Ju_���w\��=[�����?[hw0����|��X�o��)�}6A-	�K�캧e������S� �:6�B��p���}�NM5qi�����3+M1�Rk=m
;�
B4U0x��LV\�:W_����(q^�[5���35<H�u�O�,Q�A�6B)pC�Ƒ���Ɠ����|�Z��vn��o#���;�nf+[_�sz3����1�����e�(U�L͒�}(5�`��|����^�)(����y1���3��a�jm��2��b�X�S���\keW�f��6��X�^�w5���n�Th�=�T�K.�賺}ul��F����H�r��x��E��'����u_�̄`�c���H��@���l�ڰ�orS �lXD�r(�淗��x�Ў�q�"�C|rE��?KQ�r: אּL�8{��-{��<���O�96q�g!0���{��/u����æVbč8��k���x������[��I6��p��M�k���t%q�a̱FKZ7�Ô4f�5�"�r�f�oƶo	ᵄ�3逆c�sx�rR&�OIj��jp��u�=���߅�5������=|;c�
0������ܶw!���!O%ITn�DJ���gZ}W�4a9#C�)�N������q�x�5���=R�Ϊ�c��*5�)y��D"A�0a�R~~2����ͭ
n2�>�7��jt����,���ƾr/tQK	��6�=�bX���5��d���9�d���-{����>���Bܴ���kAR��&�䌧\Ѫ�zZ�G�N���ȸ�^co�θ�;Hs��o�P�No���&����[���RI�*�yjAO[	�=�iK�m0��D��8�T�8�n����2�H��L5�roUN녣ԳO);7[�F�{�j��V�Bu�ځE��i�wN(���WH�Qh`!�:�0�9��������p�r��j��|�:NcS�[�7t��kH	.$"K:�~�H��h�H�}>9�2͉~�:;�8Y�r/��Ӕܲ<�aF�>V`����Qd"?��i)�f��D�
��������0O���	.=�^�p~8q�{�y)�@N��E|���'_i�kߩ��O3�F^3g�t�Jm1d�Qo&8�ϴO\���$�����WJ͆�}b�4Gʤ���WhL�J̢�������K��c�3�GSW�2 Jx<�	Q�{clqµ �25�
��q��f��/��̳.
�֩��)6/Aa]�o�� �
-E���u�DN�u����/���ar>���V3���Ue0/�ɶ�Sj�4\��sE�N�.ؿ����Y��˞>�'��.~�
�?D�����rߨ^�~��F{���/+��b&�����y�:���V2�nj�n���gB�%��-6�:G����T7;o낗 l�v �]��s�r���}R���R�z�4���l]��M������ǳ��EC���_�~�h��Tɼ-�]c�Qo�6Q����j�\-�[���D��э�>�!�z�`���j���@��Z^���+�DS�s����;6��Rx
endstream
endobj
97 0 obj <<
/Length1 1421
/Length2 6386
/Length3 0
/Length 7350      
/Filter /FlateDecode
>>
stream
xڍtT�k�.)]�^@����)I`�Q�!�A�$�U�E$�Q�鐔������uΚ��y�AW�[�nQ���|< q@AKKM �x@ ~\ff(��G��lqu��a���A�F u�`�O��>�OX�OD�A ����"�jh� �p��Y���
�w@ ��}�l�>11�_လ�j�Z`��	Y����m����R�I: �⼼���<`'7���4;�	E8  nW�-�0�v��Fƃ�8@�~���vO�+@*�6�2�fq��}5M@�����ہ�s7 ������
����;9�a�P�=`u� :ʚ</ ���t;����`0�l�t��9P���H��ٸB�n<nPǟy�A޲�V���!�p��u�� �ݛ��d����0�?�fk����3�!��QS��T�����  !����(�� /ޟ���!��|?�H���pg�	��� �p}�� �����߆K�||�-�XC�0��#���2r��P/��� �����I/[8����_���~�g�a���l��p/�W@��!�*(� �����c���C���'����~c@^��8<�Ђ��ʰ�.�Gr��C}s������^�_!�7�������7������������NPG�?H*�#�k�G.�]�!�WYbuw�o��\9�=���|�< ��z��2�b�E�8�&�߳@�p�� �p7�����u6O���rb�M`7�
"~��A.ٿ�P���mn#��0 vu{�"	��� _>���B�~����� �������������%� ^�/�_Ul�]]�m�"����_ ������H�<�i<+����^���K;{��=�g��hW~�������;�6>e�ǟ��]�^�M��]����<�W^�����=AM�=��8NT��m�"O'^�ڎ�KM�G�־]��ȏ��u2ɓ�쭲�zh���/�o_jR�3��� Qg�e�"tDŚ��).x)�<2U;�QC��9n�g�BgO�Ims�3u�����a%��{��XeF39�WY�[w}�UHe�lۋ��S�*���,8\�^�P�ao�k�$>�Ȏ�I��O(��`>�>6G��7�Yև�04|)(A����T����p̊��i�{�I����;��ѹ;�!!�5A:*7lN�$qX�o��r���D���aF��C�7�J�]��G�������K%�
�YN�n>��/|��q�5�����\�+{����߰�������B	��0�Y��U�3;��[��~��L�RtD�T[��8��'j�`U�畉�;15�]���x�G��L'uO/qr�����C����E[����=����]�G�;��{�����&��8�y9��/��wCձҝ�OEC�� �K�5�s3b��Ж��#��r��N�h�񃄏���K�R9!����T����z^��G�%h����\ڣ�V��)LaM[E��r�n����VQ��^�.^*+��[����۸��\�F��ؘ)"qB��J;�ԃe]�q������''�sD>v�.Kg�T�/�E	��H޴ǘ��6R�k28bѕOe���^˖�j��O}���j��Ɛ['"T
�
R�`��Y�w@��;m5%�g�~/�0ģnS剭A�$�����������Yw����X��F�}����z�Z)��lie�(�Jg����"��q��e�ݣM?j����b�kf��;���i<�aB#�����REݳ��RM�<�&�
6%;���y�H"9�oca��rE�)'���n�=pO[� ��W0��N[���S���{���cѥc��QH~���yv��gq�1�Ϛ��9ңԛ\�oIru���>��m$Y��+��^|�n��_������>��k�������0��zѲ�;�nl�£�j_�ld������������]/
��0�x�&��]�8���hU.IB�����k������V���^�d��MTh�k�ҋq.T�ł�A��daoE��&�-���Q��&�s��ݒ_?�I���{�VP��B9�1�`V0����,8�����F�	3�����;H^�� y2�%C|�r�4]��Ѹ��UV�b�KT K����WK�tkS�'sT=��Rz&}�̓��\�D��/=����Nňq�r��f:y��e�#rvr�Z�qp5��P?�6�ѵ%��h�����*��:į����u��H���bO"<�h��^hS�K̲t=�݊�Ӯ����G�T��Q+�l���x��nY�C��-��c�S��MVP���#�[Id��Ic��4S	K�6A9^)1�2����@��i�F嬛{N�jKDڂq��Њg��jc��Psn������F<�&�n��l�<+j|�X�P��g�y��Ś����Юc|�L�1����!^6֪U�3��B�����]>�~@�έ���o+�n����Ϸ��"ꭆm�~�4B��@����e"���W!��	U�	gv󬆶���{p�YW�4�L�N���ZUQ�Y=o���U��(������Y��.��>�O���-���r$�x���+�Ȏ��8��	R�͗*�]���ab_���{_�+�T_	�;���\,�@�[�ޞY>g������$���Tհ;�Q��B�I�"�ԜT���������
1�F�v�O�W(�aԟ��P>��U�c�~�[U�v��-zת$��x�:�xV�o��)���>^�yj�є���p�H9X���?� ���Վ� c��`�����-9z��մ�,�bZҌ,�=��ȫ2(�*F����F$��4���6�& �'��3�AR�P��&� i5��oҍ�̄�x%���9�aX������{�B�?u��oS�i�8�;X�I@g}޹
�d�~�^T��}�F�jU��c�c8 !f��Zs��Ό�@�v�T����l���W���n�͔Nw�6ņ�)���N Q���3�LC!V_���G�iP�w��os��u�S��W'h�kb|����ZX���v#̀h�9��[�U_��
Ե�K�`��c��`M.~6�J��U�z�53ԡ��x��-M2]l����WI�{w�&��֒Z>��*�?h�d!�H`�Gi�hZ�<���8p�da`�^��)+���7���� yC�p7�l�/"��7(u����=1�f�3T!Lu��:I&9�WTHP���u]�`��������DS��X�]S?�e���HV���xi���&z~� b����U��MnRT$�Ǘ���hh�*�W��i�B�-%�3���uA��X��Oq$g�q����]�pM�gh���,�9��Co�9'��������MyR{;p�s�8y��Ll[��ZJ�1�<wO���i��G����鬊��!�g��/]�2��8�	o"
N|Z�,���;����I>	�:-PP��TS��I��~� ���w+�s��7=<O�f���nWq��rI���E��,��Q��!dҌ^/b�K͡�^��E�b��k-F�l�ѷ5	�-�������ο�n��?)h���HD�^��[��?�(�=�p�qb�n'3~f���k��ɀ?�����p�4'�@Q�[��3�}�۩��t�!��L/��l���ǉ�0�F��
B�K��H��{�VyPV[좵�ߴ�7ׯ�����Ҳ*�/(�i�r�}0�ƨg�I��B�e�)^��+�9fS30R�VQl?~?�0��[�齫󛘱�֊�#�1�]��$S)F��Nm	�K��r��Lq�*�#9�h�"���AMϻ|�4T%*o��,w>f2��̮�1jѱ��hIъ�J
�Ct����1v������w`�pz4����'ˊ��^��M��z�W�&�o0W���n��Ϩ�c3]��PVg��ԗ���VKbe[���)�]*��w��ڽ��5��yǋI�`���&�!-ş�|��ȚG���s��$f��y�����]޵w'�����3u�t���1W�6�e�8iκ���HL�As�����4.#�&�&F'8ȉ�o'AүV�|\j�k�K��|�-|����W�0|?�69�y;B�bj6%��68#�7�i��au��z�Ÿ�e߬�5f}���~�`���`ޫ&D�u�c��[q�j-	f���ZY�"cqOZ�,�.����4'%*4}�O��O���%q��toM7���g���~Ȋ�[Րvc{9�3b�=`1xȚ�1TnѪ^W�����vt�~4r�V�a�9�Ն���,Sa��q>���Wԇ��wXT�3^�n�c&x������� #�9"D)�l	X>Qk�.��|H�ٖ������3�V�0~�iČ�|Dm2|��c�䁜�X�:�R�c�o/�,�R�n���*
[�t��>-�c`�VS��xl���r��ϓ�`;5��XÄ���v_j�6d�U.���Z�l޴K�;oQ����e!~c����������ੲslz�O�B�Z�g��ר��+x:E���8+g�4��&cU�P�y(�#s�v�A��^~����,\v�>{7#����-�誝��������*��si����GAt���<My|QQ� 1���Pz}�O���`u���$�E0!�C�NN:��J��z�m݇ɞ"	����r��ݫ���C��"�T��s�W�/O2�1^G���>�81*�g���`��˸��X��eyML���eZ��	d���[��A���}4T6����9�Pc��|��$'�y?���� ���29��WV�i�/	-�*�I�5Wt[�G���hp7�Uq�9��W���V
�]U]M!���IC~D�ܷ6���;#��aN�w���¨��L�눦�w���8���G��.�)�zS��v�Ř3{�g��n<z�U���������&R�=��;��S�uq3C���FX��h*aMC��&����+��"IJ��g._ㄺ��^j�e3�1�r=58��#��w�s!筝c��ڤ˦��5j�����_�ֱ=�~;�ix|�O҉C��l��w���!Z|����ޔAL���s��w�������4{:��b6GB]�>Gd�=Uz�Jle�'͑^��{�4~S{z
�؛�
�C'EOC�t&��$c��%X��Mv}a�!����̅����.�2�FՄ-n�Z"՗{D�?����q���o�.��i�mf�EMMX��/uk���j��2\Ο	��>�&޿��R)�t'̇�R�N��WY�iY��m&a�aHFmK�un>vFǊ��I��S�$�mlF>��}mJpVj�l�`c�d9���%�ٛ8��Fu6Y�Q�������n2�{�-c�����{�ܩ?��˞�9R_����K�6g!.H>�M�1>�D�]����[���y�v�o�m*[�a�=3�NK��|8?Կc���$߷�?=M�-��� �@Q>��\�еk*�$0n_��Ϳe,��I`�W�n.Hς�k.y��U�̈Z�-&E�!˖9�iT�Ů�eF�_d�JI�)�t�o8�e+̧h_����OyxA���erU�u��T�'g��_xRQ�k����^bGA����>�e����×|�g��⽌gĵN��BU"U���j�n�&��Dg��z?�SAcLu��Xs���Q�㉚��5�X���6�&H�����̓��&��.CI�7Mpd�!��T8��׀cs�u�)ӳ��V��~	A<��KDr(j贸�y%�s���߸9���k�f[�t�Lu��6r7$z��⒵��7U�~,#��X<���깷W`Ͷ�������$�W�YD�r�E�GN�Aq�8� {�-����=�.��A&�S�c���5���Y��(�C�Pr罱�v�b��ڽ7[��2/�mz�ّ8��v=7�5K�Q>�/4&~���C�O�"�:vL�uaK����X��D�m`T}?ӻ��<)������ �k=�%�Bqq��C9��~XD��^��[�#�Bп��	��r���KBu�~!7�	JD��Jʦ<8�~����'�H��HכT|RN$��o��M.ԮX�h�2�l2(�L�W�.8촑Lm��PPy�_^q�Q7Q ��-�O���9%r���n�����'Z���\=K(����4��T�M��&{���k��5�mQ�8>F�&�bg��Ţ0|�u�#_g���2Y���V�o(?lC�	�F��J�	�`E�4�b���?K���2�H���J���ѱtk��nco���blc漙A����J�P[g;�����G�MW�8v�;c�@|	�G�|����U�����������C�"� 'o�1<��������xp`%�f�͌���o��XQ5����zFJ�ci�o�~���s>j�t��d��^R�.d��@�i�[�;Ȯ\v��骽�t�0���A�(�?l�����Ͻ�Z�T�Y����׈�(��r��Ą�^���ս�|�oF9
(���X��ۀUё�}*M5��ܸIȮ�$:�w�v2��L��n���]T��߱3�#��"3�Vk�-�=D��J,�+���Za"�[/J����"m�I�$d�44�%��9���Oꉟ�o�6Hb^�f|c#4o�}���HC?
${c^�]O�:�B�J+�r�V���m��p	�uu>�ǉ���|QRt'a���%�l�@��ڢsku4����\�>0��Թ��]*�C�ҁ����Ĳ5���/O�)�e�qI�2[���g,�>JqI=�f� '�'��Aj�BS΋G���iޞ5�q���n��rҋ�E���2�
�|�܎�G�R��i׹�y-NW{$c���}�*�;��;h�H��]��ۚ��2�W��(�Ċ��z��'�I��iV�;�;�)vN���کS)�⟌5rz}�g!�q�՜�H|Ŋ��z�lS�����3˺J���� �v5����V>vʩ�ʊd��AP�˭x4\�����;�֋��mB�(���>��b�i��	v�ᓍA;�5t�6��ܛ�g�n`]bRI�N�t����P�]���Q �D%iH��������D�\T�,���BW�cֹ��%s���7����iՙr�G�6t�}oy�}��Y,=�Ʉ���pe��-���<J� c�%<�$��jA3�-��z0��W�ܕG�⢷ez�9CE69���[�"�w���pR����&X����)�'k	Wjp�=���ν�X�3�V�e�U(�� �c|,����`R6
endstream
endobj
99 0 obj <<
/Length1 1711
/Length2 10318
/Length3 0
/Length 11419     
/Filter /FlateDecode
>>
stream
xڍ�T�k6LJ���PRCw� �%53�3�Р����-!��tH#H�H7����������-��k׽�{_�fa���í!Jp���(�WWW ���@ .���ˌ�b qu��ab�
�w�X!�6+2N��;x��Bb��b@ ��O �U�`�Թ�p��E���
��C ���'�����# �q���` u+��	y"���A!��*�&a�@8���xzzr[9�q�]m��9�P�@�q����	4�� 2��e��A�����m�V� ��A`n�w�
@�UQh:C`���	��n �ܼ��+�w!(�d+��l��l6PG@SI���X�������|++���52��έ J�� +$��蹁\��7n7��o�<�� oY��;9A`7���)@]! �{��9Y�������6�I�ݝy�aPw���_!H�?6[ ��@\ /����z�ΐ?����}��� $	�?������f� \�!���v�7����� �b���Si������w�zL�H�������z��s��'����<3�4�x��'�}rrp/�/�L.>A ��O� �����*���?�ZVпz�SPf��䀼�����Kl�;࿏Ѐ#���#}3� ����^�?R��t����M��ې����n�?�������� ���ȵP�#������?WY��;��Wa�\Y�-R�\��@�?�P7%��E����f�<�
�h�ݠ�_d�?>�ց���rb��ܐ+��c��1�d�݇"��F>A!�����7.RH$��rm��?����� ��?���{�B� �ߦ?��G�o$��Q�� ��?H�c�7�X���>��(��7����O4/��_ك�� ��ÿ ��� ���ߐ�����"����hd�3R�p�?|y<n�Vnv�X�,�V��*�l�_5��5����5���+rP�rH��<���;7���׆�^U�R{rm�J`�^�q��c#z'-��u�2矗)���*��whȹ\e/�������[���^iۖ�:v���K��%�aJ�a��X�k/�m�O8�`�^]B?�����'$UC[5�?U��T#�5����h���<�`����e�����\��7޳��j��������@U���c�׈Ǌ�4
XU��osJ<�҉|�����t��t(p����(�n����3��p�(��ӷ���)���]�")9_��F��%��tڛ���$���\���i��-��Ϯ��髗�eu#�S+A&\܆������  �e�2o��Wvo�ij{�����}N}3���vn�;mʶ8����jCL��OA���j���*��|�~�D�@�.C.�����2����.c9x���h��D]�V\��؃K�=E��>kY�^dV<�8,Z*z�ax�!�^�,c!���^��2/��N�ko�c�����ʽ�g���{,W/CyJ�&�����cg�����"�n3o?嬼�����!f�Oxpe�����~��[V*c��?4�΋��l�T���}�B����¬��⥭�^)��Zs�s�?�F;�t�F	?<�B�}�ǥ��x�T��{9+����+����(�ݾ45�%@���׻��euV0��d�X4�'s�9�'6��0vR����1�*���L阆�2|�I�ʨ2T����֎��]�S{�x�X�U���<n����M�x����@�j�^�Fo�B"Q�o�e#?%l�f�C�M���ޏ���C���w\��b�`3�Ҩс*��Cєg
L4�ӑ�oc�p�k��LSU��tf	\���RN�)H�'ǿׅ\�ZH&#mzk"Jv4���E���-.J޳�܃��!�����C3n���������#TI���A�4�ŵ )��殁ϖ����9#�\�*��k�s�nB����A��T��[��]>o	������0��>��z�J$����8�&F��(�+��t��a����و�	>�oդxdwp��ü�<��|��^w��aN�ܽ]C�1�%���p�3��:#�{����j�IEa0�ߵ����ux�oy�jg\`��@W���Qw��!�׮�Ɏ"$�[(/j9�l9�x�}�Mgxd����p�;ov�{B��,O��~��a�FOU��Z���M��%bĪ�$�:�([B�4�E����ZQl��Q��>$ڟZm0��\���h��EC��H���8iԟ��a��{�K���p��l���!_������>\53��;bc�V�~��"\�]��pF��|1�zn����^y���z6�� ~O:��ގP^����=yk���v�B��X�*�{��x����m�M�m������2��2��p�����K��n���N-7�*��}8q4m!u�Kb���S���.��i����U;�VY���/#�4*~f�;�y;>���7#�xv��Vu{
J��kA?��E[GJ�ã~���d�E�I��`Nҧ��Q4S����m����ڣ�sx~:\�+�
K&�kYI#�-��Zy���T��6&��e0[\�Pͤ֜���ݰ�*��'���Q��|�h+�Mu��s��3PXަ�i�J6[�d=2�|����Q���&�^����Of���%J�pB�����-b~,�O�=�N�=�Il�m,����{�ٰ��I���e���ݰ� �O;�{r��U�J��w�����![������Cz;&�Տf�N+�.K%���Y��ʥ^�Л��p��fth�Gy:�:���~ț�WS�=���q�2��]L��[�Z���/S&�M�u��嗒�FQ
*I,b��N�]��<�*���������F&�z�T;u�Po���1�y6���6պ�䎡긁����R���`���N	���5fx'�����O]��T�3;m?J��8��<aw��-�(��x�^�}.�(	j�ӟzN�Z�BW*��g�=31d%y�D�Vu�ƟP���������[���B�y؄�rH��EC9^ƷNhc���Ӟ�T��6�a��V�����є؂��I�R�-�O��v��(�NS9;�^jXǸ^5m�+fC^n�X����LQ�6aN�"P�'%�����$Es�=;c��!N �A5%y�8u��(��Z6AD�|+!)�lqL�Bla��U%��[
�r;�Yq990�zHՑ�AZ���]�o��G��I譆�W�w	��Lre�De�MCrf�y�h�͉�\�f(�3':R����3醟�����ގ��:�n�{�j������#|��j�穨��}�����s�
4F��g�0o�+qbJ�q��{7�=˒*��p2�/���T�5��M�5}6{k[�aM����؅�n6Lt��ݥq^%����2��>�eRi�qގ�d'�ø�PN�C����n��LdO�4��@�}1���sB%(	�t͢d��4��3ܭ8�M�A�^{��h�Z��x�D[�lot��>����0:,}lBߢ��E+�f�N`)y#�yB��K�Ad�q㼃������3��n��9y������.t���WrXآ+��:`���W^���d�k����1_�«)��X�8��늎�=�T�&���t���ёvrf��_�ݲ�����J��n9s<��Nr�o��v� �E��@h�aO�;�Q����S��_�;��,�����S�	����f�PqL�l�Ɠ[BNc8�G��y5ʻRғ��p�ve<����z�-�������"�}�G&�S����i�hFב��q�RK��e-瞈��DG�!��n@*���*�
�Bivtg�E����g��T`�b,N�v���`��h��/���l�h4'�W����ާn^��W��F�>�Nh-EP�AFfL��m��5�����4Vs_�m��,-3w��rS�w��<O9Z}���V�c"�I7x�Y��kLu`��CQp�8�D�r�1w�ʌ��vŕ-7��E^Ɩ��Տ!��{UE����H�5%�l�Q����T~���{eI����X�A�	����ٹ�'�!��G}n��bL(�$w��
H��tK���ȋ �M����ם�uۡ�7�܊�r�K���X����N���u��bKݍzձ�\~�`��$~1�p�T���7��s�GjL�Xa˂��u�E�+��|��m��_��*{]Z���p@��|�Ɩ!U����T�u�i�`py�g�$:xP�g�g}p��N�C�P#�ʦ��X���gvZۯ��0�E��*!V���1���B����fm*�`e�C�-�����J��"��`|�ě��O,��u*Z&����0�^�6o��8�la�H��
G��	�%G���T�_�UI�㬧�ӥ�������V����_\����g�&��gpH�EQ�|0�;��ѝ݌!s=|��A�/������u��B��nzO�"��@�X�\�#L�C?ޚڞ�C�d~Xdwd4�]�����wYfJ�<����M�V��Ӡ���Z��&�Arc���tc��⽤O��M����s��e	��t����#�"���+����e�84=�ސ�۸-��3��pǰ�h����R'~�����=X��řnc'����/6�Q���9[�]���l�{�mh��~�[t�-J6��D�!/4��{«�
u�Qtp}@t	NVQ���8�<R�no�*(5[6�8/�K(�h��"���G�-ú��F#\�lQ������Ã���m�7n�B�������]�}�#V)��W5�t��d#��k5a3O/����n�`�X4���^�U�z�0����=�y��&�h��d�=E���g��!�,�e@���h�W|���Q$�*�T"+I���{����"GG@'�Ϧ_�Ù�^�&�6˩6^���lE�®�V���[�9(�OnL�g�ڼjDNb����|{2�9�_>�O�I�}���1�ЃL��Fn�*�$|���i-�z��Ȕ�U��"������\���//��~�W�	|$�����Y:���5�B��\��		��(��Z�aQ�	E�p[����"R�D��5��f����S��hK,�[�C$����ͺn���������k��n��������Z���^��Ә�m�^�/x�W�{2���gS��n�2硙EKg��%y�}�g�y�F��v/²�:�\�\4ۯTn��w'��zV�ɫ>A̸ǧ��4z����vM��`��u�r�i��	&��[�,Ϋ_JF��9J�x���]B�'	6ok��:z�7�t�F�H�����E#�AgD��%�(I*>���I�!s��1��ڇM���(�q4_�^V��H�k#�>ׯ�
��]l�<�B�H����A�����?e�g�.KI�;>m͛�L@�m8�#��ܒj�[:w����b�R��?�}b��n\HG��u���A�e}-<md��.1���tv���<z�,�jXLJ��c�+5�0�~nMVI�{�g|}T�ZQS(����;�S����$m���]����ʈ�S*h�Ei�Krd�ɸA`�YSٕ��E-0����J2�&+�z��A]���?o�/�sR�F��hTR�4h`�x��T����z2_.�@9�w���,b�8d���2�/lG�DM��񾀩PUс6Iz���+-�;vwˌ�b|�І^����f��~����3�O�n�=x�ꝯk�޳``}���|K��l�d��A�6^E�ţ;c�%��E0(+�� U�>�]���_$�����\�s�N�LftM�ج�T����oW��N۷|�63���M�.�Gk6����~�l�K(������Y���Y=Zfسٓ������h�^خV�MX�[	�67c����S� c]za�a��+fA��8⑃�ĮJ�/-{�i�־�p^��0?6��++xǜ^��-F�o��(*G�w4bV�y���u����5�A�3��Ea�A�wi�y�
��&���z��G$Y>�, �\�&j��-3rg�E�3i������:U�6�_���X��B��ΧT�s`��t�w���{�۵��y�#
��,���26w���q(Yt���Xl{kE�-���j�S1
MԵ%�g���f$Ӯ^m&���|DE�c/a�)yS�Mg�-.�eZ;��{:#0:����\f�KJ���aZ]�"S  E�x_[�#o���%����&Y�1���sK)L�MY~\��5���؄R_��8ҘE�m�uM��e�咴�
?����z���m���3��#���b��g\T����h��D��b�.xcpG��&�RW�IN��ԟ��12=ύ�T�%�!�X��ٷ���>l"��+��6��ƳG�(���������Un�#=r��oU�����Q�$牿S��B�ִ���|aA���|���@�?�Gql;eq��F��zY�B�"��iۗQ�8�o�thA�q�ɿ�$,�T
�3>�?��ߔ�p�Z��G�T�,Z������d=�@7l�S�(�q�,[l��@���c,�2�/��5�f���N������PS�_x	���P�.5��� ��y�P*�g�����N��rf��c	����j�MX��v#W8��P0m���ό�E�?��y�­���ɏ�,�qA���6�;�(?��|qM2Cd�����`��P����'�S��Af�
��v�Z��������Ȼ_�n��$��ZI���LyL�Ϳb�o�U^�'�wW×���뽘��o^kH�;��Ж��[��_ϲ�ˡLș���}3v��<ҵ�\E-MYVR�>�c�7�͕܋#�ς-�UTsL��%��06KC��%O/��P0�>�r�t��y�ٰ�Qi�c��>7Ƽ#����}<��8Ѥ��Eq ��*v�"#H�������s�"]�����y�XGˣ���9���/�o�ǎQ�ؘ�z�8^�:�rJ�B�}��F@%����7[+JP���F��L�^�`G��Ƒ�ؓ�9��]O˛X<@����2
�U��'������`�#5��	��d�٧����}*V�N�����Mޣ)�~G÷~��>.��9���l�!ZZ�W�uOV
�4�)2����9M�X44�/b�J�%R���:��1�s�tM��ǭ�X�#�O5��\�BY��o��Aca�����֙3������h�#�3Tcm�� �c����'s�g��ߙK��jۮ�y���K�tc�P՜_��a�;I;4�7��k����¥*ݢd��H�R��!RW84�T�;8����}(X�dt�Y���Ep3���tݽtƮ�^�0�$χ߹l���!5 �>lqo���Y��	o��<�S����M�09�fJc�b�JH��q1�X6���k����u���U�C��:�C=c@z����&ҋ��ѻ�G%gF���.S@:H�f�͙��p�K���
�.	D)I�3AC]�� �T��yF�/g]��'��(���ѳ<��fc���~rT��ט~mS^F�$�f�רy# ��F	V��5����\�2Yc�~�d9��H��(5W>ʋ�q3����*E�G�O��̓+A���������dx�4w賀?�ZŒ�^qH�aK �����ڷK��Ϳ���Uy�FQ�Kx%꣐jH�!��o�:�T�A5{�Y�Љ3C�D�a�(z���q��� �	���3?E%���}.G�ȴD�O���q��<����%��9�+zl��o}����>�*�5����H|��h��>W��hi�of�0�Ϻ�������h��0�"?�#O�ֈS��Ѓҩ��s���0���~��s�����rдv+C��ϗ��OA�.[䡹]�!���K�g�IA���TR�E^�7+	}ǒ}4�c�iYA}y|���c��D�];����~h�
0�r$�p�ݘ8��w��dvH�(�*��xj�>�L2|�u�q���6+}�p��������@����>8���W���>�]GU�1��x�ix5Y K���3�����m�ݾ݅��������i�������*�Ӝ����'��������XLGcWh u	,�q����}���'�d�|���O q�X���TSK����2�lH�{C��4T̮K��h�E� �t�+�S:�%<+�u��V��M����P��ށ����b���݅Vu�F�i%o�Wo@M?\U��&LgWqM��㏟P2��)���W�|c���WVu+S[ۻBK��Œ�R}����lY�����0�����5��#8��/�+�
 Ea�a��x��w��.SI�he�gt`�Ǭ�*�1$:W�K���뇽�6�Y}��z� %�P�P%m�m�8o���ާK�@��u'W�Na*-Y9#�y���2|�I� 4(g��{���
���"`�����f�).��7S��w���L�{1g	v*��s��e��9���9د4��eUCq����K5�mhyY��
(3�b��6�	4��HÚ6]	���Og7*X2��N�H=H![-���g�J��Ww%���0�<�C�dD���^L9<Kx+�Z����F�B�_չI'[���*��\��8�;~��]H���'��7K�y�2��%ׂt��;�����'��R֝�Ŋ��tZ:�V�[zF�����H��o��k�U]mHT��ѽ���P���S�J��H+�p �.�{e�&e���hr�XTY�����D��]�9�T�E"�{4F-q�+?V'�.S˴h�p��.�b�DHm�tiy�=�y@�a�OI�F�s��rطHU���锴�G#MNz[iwT����� oA99���b�����j��[VOQ����[Ǵ�k��(�(��y,� �vU��Z���]Z��m��Z��կ\B>���~�lֱ�RTR���
t��[��Tn���em&5=��`<�A���6|4�������\+���7�_�V���W��E����2�3��]��I�/�1�:Cr�jO�w��A��ʙ5+����wz�1�-4�Q������z����A�`�e���;z#�r����Nv���N�<͓mG8�LŴ1����BO�����b1rY6B��Q�����п��m�X_]��CsM^Z���\��5�И�����vn�h�ش�)���ٜX.��`	?��ܳ�}O$C��t[b��T%q8a���L�7(��~��/�~�v�s����f�ނ��xF��ޯ�ǽ�.�F�:)[��?���:=[Tz}-�״s����D�fn^�X�톱g$+w�I[(��|�$k�5��xz�?���1kzn�P�`�R]Rii�)����&Z��;:jh�i�1)+:
XL�0�U��͔��Y�f��.����lBќU�����l���@�A��ܹ��rm(��I,2�'���a�ٶ��nK��>R��OIyi��V������B���r�Lu��
f�fc3f��d}MD*��;أ`{�m0WD��VA�����7Kɗ�بe�������A��rQQ����b|C�刺60<��(y����)A7է�b�]I�U����=Lw��1cS���<���b~ܣ�R�
,_�W�}ֲ�d�;�Gg�_�Q��c���c�y�|7{��̞{��[��]aJz��v��wQj�R�����AY\n�Q��>��7"���}�2�g�#�CJ2����/q�.�.
�GI��(���W�'V/�"`��I2Е��_���U�҈�k��a�����3�� �ϗc�,���iexJ�Y���'��)�_���@E�=��h��{��^ž��G?�LxbT\�}a�cp���6���ɍ�s�$n;��:V���BX�6z`4K}��H,�}ԅ�����ln��$���eB����\/�I���Y�-��_��gB%X���$��]���t>_=)�]D`d?h���õ���9|ޛ`�ՙI��#T߿��#�o�QRX`��B�_�;~�o�3��6�2Qm�'M����wS��x�B�{�T�s6HD��Ʌ�I*[�!�=_&026�}`	����;M�:oB�k�;�q�|����'i�P���t� `�n��uw��|T�b��	�~�qO>)=Yg�i n8)�F�2LP|��,���� ��#���ۭV9��7D\=+�"�4��ǵ��Q���Q�csoz�Z3o�r��o��<;zr����d=;�/�-�|L7/%U�rԔ�H�'�Ѣ��P�2�]�y�V̬LS�#��/|�������12d�qi���%������b�
�%�������
�0�'@��ɱ���Q�N����3�>�=��1Ґ������0Oz�PcM%�S��E9�N����h��@��]�ք��*oB���I|����+<��/IN�+R������*��>���qήX����ӻ�[ʷb9�=?�mh�sHtBO\��v:'^?PB�{I�H���WT�pzRHQ�w�^�$��[�*hU��0T�n0�!�K��##�L4���);����zC6���!7P�K��Л�X��c��B��}������@��!͋����.��^��������I���)��e}��(�)�P��Ǿǌ�ѻ4ߏ͍y�Z�Rz)��_�?~107h����i&��<�	a�Z�,}�RΈ�ь�v%�U�5����z͇'��p�R����4�L�������˼͛\?�h�%���dN�с��P@BQ��O�sv�pb�!�Y ��	������3@�|��}���4���{�\��gL�K�W-��*�ϕ8��0�.�OQ)ob�x3[�/���|E��D(u�Уu���!��۾��K�T�t�L�2��fL�E����6�!4A����]`�@Q�}}ڲ@�z��Y�I���}
#p�p�����-,�=,ݐ��m+�6u|�Z�����i���r�Z���Սӧ�%+@F?�v�	[U:��s�'Si˰��B,���ǧnΥA�lw.�'Ux,�Χ���R\YR7��>v��Z V�}H��Á/�`T�Ns����l+���5M\��uq#��>K<r�b�͊�a�3�O,Cqy�V��Q��j��-�'������p@�=&$��U�{�6�"#��-P��cF��B��!�ٲ~���s�@v��4�PbԺB�%�]���JZ��r�0LS���6�c*c*�Z�a�(�b���߲�������Qˌ���*��w˛��g��ے8M�g��EIϡ���z5��6�j<(S��,�h�P���N'���lG-L�}�uB�G;���ƭ��<:֏�)�����Ȝ��،[i!]7k���d���Uq�Wy uLN3�� A�K�< sOkr�]�y�g�ڮz�C�F���/���/���O�R>�_�}CT�]�[���g��I�38}��#�H<
endstream
endobj
101 0 obj <<
/Length1 1447
/Length2 6431
/Length3 0
/Length 7415      
/Filter /FlateDecode
>>
stream
xڍt4�[��Q�%JF��`�޻���`Fщ�	��%��{� J���������]��5k�f��-�}���������
�����@�@����� ��y�ta('�_0>�>�	C�E�����Bcr�O��;@| ��HP���;"�D r�@����C��L�o7��=
}��K �5 $,,��; �u�YC� u��>���AXà(��`�G�\Dxx<==�!�Hn���'���hC�P7��a����7>@����� lQ�7( 8���p$:�nu��(�4]��?�j8� ��O���%��C���.�7n��9A�
j�(/' ���qB"���	b�v�]9� ���	�Ei�sA!��0�_y~�A߲<�F�������꓃�A�����󧳎p�'����-nc��������U�������AQ 0PH�O� u@���y~���v��6�~�h~�.�-��fE���"!P ���������@ �5
`�����Ɏ�������� &@��@ ��Vfhy� �N�����/������������ � �\�� .^0 �� A����i�s���>���*�OFe�-������&��.X��6����@�����}S h�����'�w��M��������)�;9�6����f�3���/���Q�PG���߮�?�����;��UAχ4��q.?7��C*���6O`(k�?J���3�`p����AG��eC���#�YA�;��A�g�����P����y�5���8� 77�7>Z��Bϭ���<�p
@s��"����[���=����XAQ� �h'�����_�[�������tm�?P��v
a-�P�|V-M�ɵ6$���~f��5Th����XN�y�5�Z�0�	R0whՐq=�^�>�]�}\����Xa��U���ɭ�$�Sj�)�F�<�TZ��.�'4QDmx�6v]:L�OYj>�$y2?(UT	m�hi�.�R#������zgգ��A�����5��y�F��X�9~�g�ҧ��y�N���@���}�0��h�?N��|������_+ERY2�<:m
����w{�W8�Y���)?�g|+�G���;���r&���"}m��Z�M������b/��:�mib�bxj������N�Cz��1�A.t�4�����x���"(M�+���0�%TD�p*$�u)��v�mI�I]z'����]�q�X�#�=�=F'zy�'�W�>�����e$�ϧ�R�0�O�LI���͕�ViD&�wݛ�9��} �y_\)ۆ���jE��W���7�R�<n��cFv�$��M3n\Dp�b	��Зϑ,���؆BGM4�?�>cVל��KOk�im�|#K�'�C߉�/`n3���
�l�\��FJ�	G�:[;DH�^H�l��8?���kn��^���gf������&TFW'y�uf�OG'������ʠ�����ٲbv�N�_��{��M��ifg��Yr <J��]�b��{B�����s����\ �)��?Æ��d��ZY�\�����;w8c�E��#���Y�W��5�b���-�-��L���bw�k�$`�UpH'^�	��Ž�+�b��w�OD2�.���1�0X�ȿ���EU�CW��tl�0
��G*"Y�|��D�4E���)��[��3=-��=���d�A�&}�wO}��ʋ�/Mq-�囗��wmC�	��m2�i�\��%���}�9";��|�����o��T^��󪆼��;q^�Z0��ʷ[�s����(����G�p����I�B����k�,awG��C(���Z	FR�:u��؋b$��SI5Ui*oM�ݷ�tl��V)\u_�&�]U��A����ii�\	M/��C���hN��Њ�z#-0_��6-��6���)?��]�%mU����+PJ��^P��P�i����E���L��G6��q ^��'ݒS��}�1o�T�;��0��>kKY�@����'|���2�I�>5.`*懕��ը�������c![����nM���l\�O!�b�A��SS�:dJ���Z�դ�6��8����5xx�nf�J�@��Cո1"8�AJK�%�؄Z1lz��(6Ov���v��:��I/��3'���5ԭS��ya��#�	�xu���x�lB!ܶ�F�S�Z��y��é]�Ehuf��e�~�׬j{F�uƌ8]�..��v
�>"Ϋ����q&/)E���MK�OV��˭>�`�lϖ-{R.��z�]3����-�d�|���"p�K*��s.g�k��9ѯ^-���� ��kA��#�0�1�|���P�ktPyD�Z�?Y����"�O��ϼ�kĭ��H��Q���ê�4X�7���JD�񈠪���f�k_���!�I�u�G{�O���AK�-�V��3��H�x�?`��VN�)E�|�1Q݀��?�i��/7D�K
D7���DW�[��"����'d�Ѐ�^�^d0^3ӳ���U���۽6����~�rmӊ�ߎ�x�����7{~�@�]�w���)�e@�����y�j��a]�_;��#���)'��z<��bz2F-��q��K�F9R�;����9���ǯ������_oԭ�>�t6�| �_Wݛ&��)�6�x��t\.v�È8�L�H�PL���vM<K�Bt;��)Y�y�����ՠg�O=���k�2��r�
���Hq�����.����+��,��ID�p@`�����Iz�Yyb�Wtt9s7fnA���_&��~��l�EP�]l�'�DC{����j�;+L��n����e���Ⱥ>�)��!�J�T�C�(̄���6w�i/��,	o��S��Kw�����ًJƎϢ����x^�r3��4&�g�=�|�90�8(^g�i!u�\<�����H.+�.�Q�5�:e�Q �y�[����i��K)����뼚[��Y�6)T�W�W��^�7&�>��Ù��C/Jd��ÿ��R���o�!w�R1"��8��}9XU8*$�fo�d���9�h�է��Bϩ�$[=}Oɝa��Ow��ًc<&H�����R(S�Yjk�LF������� ����Ɓ{M���0 �����do4�=�kv�}�Q[,�[��	M�T1;�ձ��Ɩ�P�S��W9Z|��K��G,fn�����,�Ɇi��"�}N�}i|�+�R^��R�O��V�yrCfG��R��4�ĤC;(��O�G�	s2s��w�v$� �
t��.4ݼ&r�֤j��TH6jÄ�*9��ʫ1uʞ���~�~)�/�}�g{U�{K�� �)vH���v=�m��/Gh�A�� �k���8�[ٞI̬��x����O`�\��G��TO�A�(Ǭf�/��b::J�y���?�Լ���q�F�5�(�=�.0ms�wX$����hǦ袽6��$�n�
U��0kձ��G���з$���P��{�K��3ק�����[���8���֯�oJ��G��c�8F�+?>��↊N���b���L����Z�u�&���"����C�j���#�[~�Y�ab� �PpQ)y"^���ǩ�0|>pi�e���&���Imh�uY���FW���{���S"�6�E�=���;?�O;�Cj�tEy6�>�b�|�;bg���[�1�Ǘ�1y~0[����;����a�}M�+���Z�72���7�y�j�o���F����-�å��|�	��l�DE��#��%�ӡ(�*]ll�;S�J���L�p1ǂn`�M���9_�n~�S���Hѳ05���{,�p�R�t:�2������+|������Y���T�Y��֤����n
��ԋ����?����8�sO�M���گ\�#�q��o%�{5�f�S{��*Z�^_Ie y2r�){ecX=��y%޼�r�E��=�w�]�����V�����3��Bz��,�3<jPBp��t�7��".+\,��~�*�FW���[���z���冑�7Bt�'|1��m�Q²�L�{�Y�6�}�.l�OV��#�ɵ)�P��0|(J9�!.��7�GN���<<fxw�5��hQ}�kKw�hC�?��="�*�5��!¡�o�_A��Ɠ\
#O�T��������:�(�S�[�V�j;�Y�Wx��|��؝���s��5;L�:j�M���G4���ߋl�5.��Bkϼ10�6�%�.��^R2!�\wS�ɑ}�/ƵR/�F�<Z>E1��MT��	!.�R��x%!_ƹ��1�E�_d��I�U��徹|��Y�I��ԉ4Z"Z"����fj=�J0�H`K��ɺJuE3o~��d�?:ԗ0%�B0��	"Z#)w�̼��<�m�PDH�����Y���g�o�a��-�}�%�K�(f_� +�X~S�\���vc�@��a0e�	bg�v���.��\jiu��l�0W�?7|k�MG���=�Jη$��A��ũr?j���eo�`��&ixRX;�B�ɏN�P��eܑ��z��wͽ:}�71j-�b#/3l^�<�ֺ��]�[1<W���y@a%�%S m�N��p�I1?�{n|]����n�V��	�{�����،Hګ���xjˁJ�m��rw�`d1� x�~��K��1G�b&�������3�7
\)Jۘ_I��,_/o�F/��$-EL���Ř�dww�!͒ku�3yj��g��[eq6k��9��D?�1p�w���,��t��\J{�����g��\��lu<��0���I��6����s5���j>�1{Sfu�¶�����H�������`������ai�R��[�s����`�����`e�#"v�
q�~��3�3�5o�MI����t �H�b1]�/���O0�9�CO����^$N�œNT�Yk�S��hW&Ey͂�V�h�_��<gΖ��%��X��Q�~)�EZm���G���{���iB���!���O�s!�?��l4z��2M�P�qUS�j�����DvV��2)f
F&�������|�-��A���|�����l=�}f�v{u�� ~�3�i���q�7	�]���-I�?Ҷda�����X&n�-~��v.�C����O��k_!��"�| ~a�澅�;*���˥�[�	L)3i�g�����x{���U�e*��A���{���hQ-Qb.{8�*R�*0����1�u�V��c���C`����yz�"׎+��kZ���3�
W�O��o��ne6\F��Ș���b�E�H݇32D���)�/.l&q��5ٶO�qN��`�.��ӿ	��s�pƑ����/,%���s.�H��]u��(;�Ȋ�\I���.����y��%fī|�*Cy�IWr�q�:B�2�.�2g�4;���$�3QA��!޸g�X�2)NG���P��=Eh�|�g��"��f4T��L;�^+]���.����Е��T�|��XX葽-Λ�4ʖ�@k&���`�}�^V;��O�u-f��?3\"4�돰<��r�(o&��]NF(���1�f�����S]�]NbD��a�t
k��x''�>��: ��4��Ls
��e�C��l-��#"<��a0�M�F��ُ��%N_�@e�2�sr�f��>���j�ՠ��|�z$��Ѵe�3�d(h�o���Y�vu�K�\ǉS�9x����ŒՒ��V��?�Ly�t���J�d�N�����hr������d3����{�]sލ0�h�8"{�G��]��1��з>'�|�g�z����.�{�׼���J��܀<����v���ē�7��G�=L���R�1^��ʫ�*;Wt���v�O�6M��P��ܵ)����iJL�N"��Svj�ëV�1���G�	�I�MKz����������,�]����uT��C�n?R[�ݧ)#�c	����g�&�N��)�%Ty�9)��P�6��2�oZ;��SrhD-ufOOa?(Y���Ғ�H	~�D�B.@�ȴ*���`a&w5��:�J����(�H�^K+)g�K}8템ǌ�ftS���E�Xq��J�;ݕ|�y��1��T��00iߋ�G�A�ҲGG����︱�v��i�w�� ���>�c���0t���m���}�B���(Q��]_�ꓟ=
�f������"a���cHK�A�V�u�(�
U��.�=hÝ���u�\�rf��
�BE��.�>���1p�9E��9���,ʚ�����I^�F�$8�{��3k]�m�1��Ź;�G�����@����,S����'���Q���lK�
G�O:��J+�3":��D�8��t���:�l&��ǉ���U{�*_� ���jP~��r	�7�d��r3�B��/~)N��;A:�R{����'���=["�â$��w��|�Dm$�76�e3󻟸�X;X}�^�d�I�'�i����]���k�D�96����vx�Ǐܩ
�Z
 }�w֪�;;�o�*����R{�6B����ڣ~��k�]A�z�4!͐R7+����Q>	�-Hz��Vx������������pRMn"������+p���M��E���hWН;HKS�F�</�)/_+�XL�C����֧�V�l�EYv�U�n�u�`c�}پ	��5��(<�`���g�J�u��քc�������e���}|�*o5����y��KG�ENn�� �7��)IP�H#��K���?��c\@��/n�GV�0�E��J)��i��ˡ�fN.[�>T�YȌW�j��, �}�E�%�ĸ�.���o�=��J��{ R���j�/oS��؍$�֌���=a���H�C3b�B� ��0�|:�g���������?���������H�b0n�U9q^�ӈa7�i�.�K��w���8��v�3���h�)'���r�S�7���Ӊ�6�k�)6�ːAz|�\�|�Q�z��α[�N
��S�k��Q�8��Qdf2��FLZ��_4�LEu�iX����Eר�Z�T��IL�Z�����Eٝ�{�;L�Q���~��9޷u�&�++*(�Lk�v�b3�A��}u6u��	��A~B�6�o��2R�h:z*S�v9��� �7b����y������X�^�7�ab������I��X�᛻����N�3IJl����Q�a'%_N�,�Ո��rU��D�?!�TĻ��/����d�^��)�VXm���%!Rȹ����a��A��>�#��� ��z�^��E�3�J�jߺ`q{gx>��h$�D9:~���Bida
endstream
endobj
103 0 obj <<
/Length1 2167
/Length2 15531
/Length3 0
/Length 16849     
/Filter /FlateDecode
>>
stream
xڌ�P\��
��$�q����.�C�����;��%��� �!��+9�����������昲v59��
��)�(�s�c�g����IqY����U-�m��#���,Av��0q9��D�����@v i �������������C�#7@���� G����E@���������@eB`����� dt�41��9[ m�O41���L,��������ٞ����͍��։�h�OMp�t� (����@S�_��F��1�G �ZX:�K�2sv3r�6�&@;�w;S�#��p���,@�h�/c���] =������@�v;���l��<,��f�6@���,���3-����/C#'л���������ߙą� F��M������ى����/��y�������h���W~���@���{0����v 7;�3K;S��H���3��Y:� �D�m�.B�#3:�99X8Y@ ��Ă������L���xك�f�$�>�f��/'#W ������O��"&&����3�hni��'��h�/��|GKw����1����I�}�LAv6���/������0Ϳ�W',rx�1s��L�l ����������RE#�'��'����=οH�W�?D\�=T��j��!zf �����2�1��1�ހ�]���_Q��f��&$�bc��o���������������r����������������j�����C���}��X��]]K'qKw������ſ&�?�x?����r����y�bd�?���3�~�V��;�/����:��ܿ0�}��71;��_����0rt4�@x�w��bz�[S���`��9�� �9� �@����� ���_�� �q���8X R'�A�z�T�/�d0(�A�~*��L�A匿�A�15���8 F�E���`dco�G��nm�G������P��d�_��z�Y������� �Y��ڿ�N�}�c�~�������!�����'�G��;E���
[�����/d~O������Y����c𞤍����?}���kb���s���򞔭�?��n���;	�?Z�w��������������*��T�����?�U�?�g�dc�d�G�wr6�S"����F�L�(��%���[�������W���7�?d�D�����|���7��u2qqt|߷�o��]���U�MVA&<�V���BntS��i��tSE�p΃b�;	*�Y�2�+L��V���9�K�^�D��wtD���D�q�ow��^��$���`�)�K�?F��������zS��/'�Q`��sH�t�w�Ԗe��r���*;�����|��ݵ�ڋ���*�A"������V��=<6��>�72@�5���,�]�g�P��F}� �9���G��������[�)��W�W?�?;�|�;+��H��ȏK������]����rg�����w���P��y-f�a��g�]�C�cg��܉-�ʆ����}p(��[Ӝ}�"�א��.���7Ű�+���55m^d�,��}N�"�F���,|ad���)��q��|9�����~�������Ь4�N�?G?���������wNn���_Xjja��^��2�}Z���~�=�.���X{MP�4�a�W���� ��'�1���,S���A��ǄT�
-��Qn���=�8)w�ҳE��\d�㫖�k�k�D�|tW�y0�J-��4eyb�aih���q[�QX�a���7��
����+��V_��L��H����(ͦ5��p��O��B�'���U��������r�m�����zȫ��`#w~R�f�5Z�s\�60�Gr�x�@�q�Mz��Ө�K�1*t����ų,,W�܏���W'p^Q�:��d��|�ET��BHKC_io��e**L�bfF8��[�,�?T�h5<����Εa^�b��b�e�Ᾰ�1�S%��M�L(�>Y�А삇c���&n��7�\�R���ʌeI�3X�m���s%�|C@����b�r�~,:�:(a�}x^�"��;��Q���;���U������.������o�i�.?v7�ϕd�Y�e
RZ)���!8��J+�-�<�z_���#$�ƾTh�݄��
�WWeH�����U���Ӟ��;�SNװ���g]5�n�B~%�H~�n�&�ҥ�1�N*�/�b�fp#�Y�k��0�̔��x+{:���)��:31ɪ)�I�3��~��VAu�6���Hó#�S\Ud�^�/���{�q�ǝ�d�P��͐B^����_��5����=XT$?ib�g&��R3��YHA30_&��7�S�"���qt]���ǃ��}�V��&CKPO�����I���k��~!%�Ž��,���`3��w��������7*���o��IjO]4�!Yqݽ�l!6�p�MC:ī=��*!�s���T���9�E�r��z�Ct�֐����(�n8�rT�2�)~���)�5Tĺ�e�`��tbpu\����aN�������X�}�ə�x��V�*�$#'�f�N�*�.CV�����7�&�7�s�]]b��u#+=A{t=2C�M1�v5�R*)S�HP�K1U�Sy�>�GQ� ��+�m�����&��#���q��g���ؿ��X������=��y%� �vǬo~���������|V}�d�^Ks�s��{%`GL�{?	�.B�\��mɊ�Ziho�������Չ-�ԢQ�* ��g��\���YY2�׹>�gI��J֌�~�=9�T�p����Ɋ�*Vl��ɟ��$���	�:D~��M���ʝ<���� ^+����(X=����"�S�[Dg���:l�=^��k��!${���q���?�FJ8�,օ����G�O.N��c�����{H%�tz�3.
�_��-�+`A�'�٤:���f��F�/A�	K"��������x4A{P��i�l�e���!2�3t_R��{��m����	)�t(������S"�r��Y�ӛ�>σ|D=%���4�Q~�l9{���^.��z]$���0���5t���������r_�M��S�-U����[4X|՘��ۧj nSG�����ϭ)-l�w��-m�x�u�ZMX���rC�7���P�*
�"�<Y���:������-�nݵ����̍̗d�8�t���tJ�8��k�����a.�Г�C�p��7���]�/J�
&V�{79K�5��e!���p���!����Ҹ3��qT�`~�����G���x��<�� �O�7����q��9��G�t�=�R�1��Ǐ˅���r��x�ߋ���0T�[��=9e���40'i���Ks�EJϧ�,c�����a�3p�w6T8#�T�x�i���i�$H_�f����tU���mG�Ļ9{��S��	9oD�����}=����)�ؑ���$)�-�f����"�g�ʍ���'u�
6;����o��2�g(�f�y�El[ ��z�Q�Iz����̧-�5i@�Js�� Q��I���6���:M1̰tq��:z��D/���ߌ�П>z�p�}��J�g�>4�'��0�n�5�4�Z�9�h�/["bk�LP�����c5�
�e�<�H�(�̉�[^~aN��:3�[�b�f��A&�	���,��*o��$����i�`�u�[+��n�O����v�㽅����˯�h�p��Xnh��[�����ŭQ�[��Ã>��OZ�?�u��I�9ʐ�fuT�`�T����r/?�N}{�]s�&��Mi�����U�!ߣ�f�9-��^�짦�2a��Dߦ+�T�@O��;��5���k01/�9\H���zi�+x����DϢc��fL0���t��l~J�1�UKKV��UGŀؑ*hw;ӏ�eK5���7�1_�PK������ț�k3󵸮�jZ�$�*��.���0e'�5cY�� ���?�3���<1�c�&Za�핀���������"�ľ^�2'�PHb�<��7��hoϲXZ*v_�?�z��qC�3<@	K�ku��pc6`D~&\��5uJ�=���e��l����`�L�a����Mj�{�L)r���>�N���q���{�w>G�Q.7��5�A�����H�UPv����ݥ݆m�6��'8��In������ ��]��o�]��2�+E�yk��4w�1)=�9��hk\LU�Ԧ�J<��2(��#�,��ܮ��6K���% ��@��W]؉-RA�F�6���?��8���b�I�,e@��P�A8�W&�Z�/�)��W��ϓѠb��"�ю.+f�������¾O��'|���Q�V@�5�³�sˡ��hGRs1���Cy�W�a�s9� .S�>�`e 7Qs�C��tS:����~FR��W�R��x�oXg�/a��Q\�:�.�j�1q���A�v��,XPs�FG�����Їx�����_�s���I��K)�X0����C;l,��6d�ڗ��*����"��uĺ�{�J�����l�d�-QWl��6q_�mk��k�3m�}po�-V���i���3���4gf,6!����d����ף��9�����Xy+]*���t�p����-��� G��#���hR�+��R�d��o,�hˊ$�~hU:BS��'(Ŕn��PB�FK��}�E��5fy��"7�^�H%*2Ę#Io��|��O��:_���"#��$^�0:�^g��Sk��.xC\�vSy>��n���!~U�j������̗� XϜH�@U)��]���`q����tFi/�&�T;��z��BM�mÿتm����^�A5<7�_��
2[��Ԋ$R���YI���:m��V
�Y�廘tpuo�e��Q0���봦80�s>-H�;�L&	ܪ�Z��?��u�(J�$�
>ǩ=}�(��
�,J�ZYl0��Ҕ;�P�KdL�}���i���{������ϝQև-C.�g�!u�mM�WfՊ�T�������8��C��1�~Q�TO���-�^7!�X(�ܷ�)>�5��^h~X$�w3l\�&�j0��H;'�����>>I��^�5����tg./�ߢ��_�x�d�R�ѯ�`�KS_,?�ڈM�nQ���\U����>��.��nڠO��q�=H���
FCM���X�j~�$-9�J�p��*�α$Ī�)����`H ՞�j�d9\g8n$lw��5��}�n���[2���8�ſ�'3�E��1��>�}���c4���H�-�A���I8}�#��(5�݃;����4���_^�� 1LM�y	��tm��D�ߜP�����þ���N�Q�W��MS��mR����d��#�+�Hm��"���ڇ�.ٽ|�v1PӸ4���F��$~f�:��{D��SB�N'���r��fV�mڸ����/�s�h+/�!�ݛ;�3`�u�^%�'�2�)9�#.찀�F�x~ܿ[���]�<�d�����T����\��V)�W+�����T"�]��\7-�4�l�;5���<Х\��+����m�R0#��$DQ�+�H���)8��Q�G!����,��J�dI�>a.��<���$�	�	s:�S
�0�+ݪq���6�A˙4����ތ���:S��$&@�Go(`��dW���f�7k��n(�t�(%�}	�(7^Ί@��
T��cV2�6��  2���\����Y��!�[�.���v�q���9f�7U �A��AR�[ҏ^Չ/��݈ZΥ��7��8��O_��g���/�m�cĸ�1����$nc��rm���H�%�)O3+��k-i��>���������)s6]2�v�b@~J}'˩&���Z�}fou|����WE��h�G#M���3i��{R\��6E�\T�ܖ����t�i&U�ﰵ�^����ql�Ug��������Ε�Øn?�d��ݮ�&�MEQ�_a�m�=���vݫdA�+��_��f��w�ܟ(m��7>�.�����/Hjk��mY沨b�#dK��%��_[��]t�Ө,�����%������V>6���[	p7���Ƙ�٪K�O{Mp���J�C��c��Z��6�F��� ôZ�%�@	�0m��m9�S���.���ѿ�J���CJ�t������ɚ~��a8C��ۨ��{�g��%���c�_�DfyXԖ�ӛ��7H�[D��أX�����ʘv�<�Q١��u[��3�#��nk�X��[ݝ���c�	�M=��4��������� ��T�9�//�J��c�3q�\����㙿3+^1����x!�XY�]B�=c�m������q�����^�4�>�u����<�e��C�])>Ӏ<u�j���k��_�ju���?�Vo��.E�����)L>O�[�z`ݲ���ϕӦ�qy�H��[�X~v���N`��<lv�� �js{W_�:i���3)���&�٣+׊�y:i~�&��짰*�ˣ�382�����\J��j��������?�u*�Խ�� �$<{�>DI�'���I��j�B��~4�3�ڵe��9�)Q�g�X�r8P���:��RA��;[��3��ǳ꛲�4��u�#ԁ<�^��%���ٕQ
���"\|�?v���
>�������\��ۆ��5���P���ˑ����󋾭�\R�ɝݧq5�%�%I{&s��c��/q�V#��N8R��" e��������R(� ���Q-"G�ɧ1�N7��1�X5�B6�Q5�m��3���Ӟ�D�C [
�iR�&$����b��Nq؂HT��5�����/^+-��]a�1�R;:$$���Ȱ_�k��8��}�Ⱦ]]�{@ŗ%ؼ��]���]���٫0�x�"�M��9WD��{�b�4�;����h��}#)�ѩ�p$�@���3�G�+�8������8F�O�̪̐���*0�
 ��;�<zMxD΄�p0�Ѳ�2b�n��4䒫/f���H�ѕ�K}9X7;�z/�~��s��p���p�H ���+�.�C���(7��W�\聍@��,z/+k�B]*Â1��V|�=	XA:�\.�����Q���t����:aԢMJ���I�;���s�(��p0[�&ڃ=�n��WeO�/v�� 䇨/XD�MVjiTeQ��ۄ�,6&���>�SB�3c����F��u��H��j
���c���16�0�C�5�OK;����u��7�*	!@�<�
�(ߵq��@ψ��RO|�M9��F��3�|.ߪ���1ML�q/��[�̕k�-'�f�~}7�_�����c���F@�|��/�=4������JrV&v��j0=���BK���>��}�.N�\��J�DX�V��m��Tl*~͞���[\�?x�6�`*z���	X���ɩe-6�Њ���z{�n}����?�:���J59#\���x�N�5$�#�<5�� �aNïB��� sۏ9i�[~n)ٌSz�`��y�D�����J���A[�:�	g��/��DWJ����L����9��>�}��ԅ�l��lȿ�=~�(�$2��mA�@:�ֈ��e�\����������b=�)y�%	[���	� &�A�>��p��Vʨem�3�b����k�	=�p}B
��@�
�G��m�-�jwN�7;��wP�=7�||�p����˚�+�E~!����g�ב�h����ĥ�,�Ǉ�%�Ex�GȪ�]���XR�âEv"w��#�z��e�g���P������b���~l��_ʌ:;�Pf�7c��Hn���wĂ�#�D��|�	�J�H�s�!��J�rT�z�3p\�)\���#,�aa[U���V}�Z<�	aG	�S0i�>�P���.�LTct���3Y�S�uL��lQ���+Q��~��Ԥ���.���:1DXwoe�Y�G��]�D>������+�7ξm<�Ŝ�{�{\�yV�I�V�i�O@�g�8�"D�u��Zf�-��X)6�7��^.��Kv�M�+]�j܌2ӰM�Z�E�%\g������j�D�Jߌ��CTW*]���1%h����7mt�{��}���&o�Ė�^x�a�$�~ǔ�H���A�PM<a�գ�?�k�@@}���}4���-Am����R�S+��k�V`:W���|��&S{FX��Aҝ�r���J���,^Y����1�#jh��ؕ�M��ƹ�s}�B�����*n5l^��dRU���~Y��7�5�Bi�����}�|�	s�;l�$�&R骡��ʺ�)��v�~�N���L*i�m%�v6c�&�������x��ѧ�����3����i�7��^ۼm�7J�Ļ�ܫC��9��]6�4כ�g��>jr�����~�nr&m���lu��on�߻��ׅ?6}H4�f�:�l��h�hFa	4�T`}�B�gk@����˯!�F��q.H�!�Q���ۇS���6��&氾��=zǼ&���iqvjeK�L�4d��&��p���d�j�}rqP��]T��[#��[U!e�� '��/4�&���8CG$E	A��җ�A�d�$�_�{T!R�1�a����G.�V|�J�,i�h�<2�Ш��w �� 1Yp3�R@EM]�����n�_?"SZj�2Ӯ�M�Cʕ%���{�?0�������Px�@�:UP�Y�U�:cH�x�(Bi��Rq.�su+��M!��o�Z9�>�b�]�vz�D�8<�r<BM�=s^�}��W3bl`/��ׯ��ț���g�k��`����9�{�m�����AK��qx[7��|�|��@�XvZ�b<k�`����h煀I��	+&�M/P6�����sZ��6�?��e�5fp�<��b&Y�%�o�s�o2,�u�C�uF��(��r�V{)�Bqu�j�0NУcm)*a�<3���ه���=�o e"Ö{�0�^�r��/%��:ΡY�kNPSZf��-�$*q:�s�W�V�n_����*#}!�Q�-����aN(l�=&�BܕTei�߻[��p�����h�
܅�#������b(z����f#(�#D�0sz�\���Q�9�Q��L��}M'(ߦ)a�u>O�����7m��@O��>K��4nͷ/#ZT�.����
�.וv���A��;Zu����A�=�w%�ـ�~�_�A&Ը�����;gb�r�wzo}SU3h���Vb��9F$xz����{
O� 	�9F�k\�IdYj��H�3�e�{��Ɵ�~��0����yH�N�in~N���mFXo����}�@�ד���So]���ըy'�i�G�7��'a27$���J�Y������32��>��Ģ���S���@��t���w�zV�`�⬜ym-�
�fӰv?���h�(Guҩ�ҹa�ڂ�̧̾�;�j�Ѿ�tkN���_#4�8��V�\��1ݲ*���芴b��u��kG*m���lkJ	�H"�&!?�Yz�U�幭�璽�Wr�H�ڼ&�yg?g�V�L�3����v>��jL�~���+J��?$î�usK�f�Hv$�.�Xk�c#qG������\d)��)a�3:���{�W��t�K���~3�jW�;࣢$/�ş��p��;��,�j�N��{a���_7��/���k��ڂ�����p�������2�▏J��/�ё�i����F�k���d���Z�w���T	��`���Bl�H��ma22y��*��O%s�ԅX�T��ؓ˚��"0�qh"���|�^ݱ�8<a��FOk@m��p3S��v��?�s�*V��;q���Qw�їn���P�P�z87�p�������#l�k�ފ��%j���#c�(����"��"lFA��Z5Ĉ����55#[�4��5����8���=��
>i��ؤ$�4��9��KZ/LC����To���P�A\t�I*eYo7�v>@y�N�V7Z�x��!^E�8�]_j�d�<��L��;]0����~s�s�*��%7qC���ZM=Vx����"�����[�Ox��~3��򖃢E�S���M�em6w�5+�J�4�~ʓ�{X�����	�+�����E��z���@�>n���v<��dkULp=}�=5Eה%1^��#����[n�1��YM�E�&��O�dD�
C���Ls�
�0�x�B�\��
pkiӎ���d4h���,e(c{d��Q}���o�����?��\�D�T����(0��k�=���m8��*��5�<�4'��,�B׏p�&�J���f9�P67E�O��^�lF��=�[(�����������	9c���wm�fV�71W�����m��G!AD���>��̬��VP��5 j:x��󏯓M�f��Ϟ�*~����-6�,a�"�̍�}�ֺ�ғk4�I�/�^&#�lի���������/T%CV@ng�W��yY��b�=ʍ�c��^��3M�gڝ�1r.G֌ҵ(��W̍��67ւ9�i�zt>�z�~1�������l]�xYs�x%�*oo�k
��j>��P�J̢]s#�^���|�X�s�+{d��u﹡�0�6f�)z�?����Η��L��D��c]d����-�c�ԥ��zs�U��BؖwV`�7���p��
!�:'�d���	G6��/�ȉEe���0��8T
� ����l�O�V|�i
�Cc�3�t�ȴ���K��5�S;�����\�Կ	���	9�4s溟�G�H٬s��]]�B5a�_��Т��WZ`xbk�8�^���kQ���l ���mx����;K�r��Ѐ�˂���W[�-�5~K��^�O���#ڽ��ͭ�C���%��6+����[	�W
�1M�%"�Ͻ=A�A�P�J�Tc=���vU��Cx$�Ow��,�UN��>y�W3��S�d5?Z3��Y�2����G����O��:[:_���!�e�8K�f�WDv��[�1픊	�y������Cqɻ��h���Lw �P�B������^�Y!wz7�|���B i��ttr���s���N���6)#�)f�G����a2\�X����v�[PK������>7h7y7r���t	,lZ`RnX<�?�TE��?\�X.�X�H!���J��L�S}*�QY�""�9�u���x��-<���Y�}�k� �X��R�x}�c�����0����
�,�h�6�h{���KS�|S��|<��30�7ݙi҅F)�[��nQ��,ׇnA8l5��P�4 ĸSp"���7YR����z^ҹ�>���!�m U��
~����O�-|�H�WZ�Z�G�.�t�{o��a`�Y&����ܡawNfh��~�̷޽�4�D���E���h)Sj�Ţe#�e%,؏�7Pb�I'`)]�o��x�H��8�r�ky��-v9�P#V/iD)�ƴx�&�"�E5�m�+D\�c��<�@����w^��R����8�&�2�T���M�/��Hz8m���Qrq�鋏b7�z�+G:@|p o�kwYJ��Xq���o�	�c}���X�v���5V�R94elg=^�^�Uj������57ZMkGJ�+��Nw�렒W-�Y�����e����}hp|E!qor`(b �uf�g��fv�axېїEt��/�5<�]�΄�G�rЂ��0t0��N�m�c}�ţA��Q�s.H�Gst�H�;;���H����핊8�L�H�b���UO�M��t�S�#��b�7bO�{|�y|�)Q�b�h�8>U�|[��e%wU��q`텏$��ɺ	�?VtDbIS٣駞i��Kl�Tn�hw�܋:�dQ�!�� �~��L�|�;��p���\M��7��U��x�i���ש��
��3��Э2G�����]w'w"�A�(]R��Hy�G*VȽ�,s�O���6��޹�|9ʷ!�e���;���	rH�ﵽ�Fʄ���η\�Zǂ1Qg��Cmq\/�./��)�n�U~�GO+g<"rz���3Gj^�1?ke�H%��^�x44�.mUo�3�	>C4�Y�Z���@5���7�iZ����r5�D^ZB˖������d��+��Vu�l4T��$��w��[�0���4S�r�F�gtko��F����Dy}؁sfrq"���!���(jJ����-l�c+��e�M�>d����C$~�)ڹ'Ѐ���)�+n+��P���ڐ�h��o���^��1����HVE۸R���a'ـ���}�x�����6\^�cV��zm�gπ{�J���r�W�[��U��J���ȞՂ����K�@�Xt�4�~>�q�F@�}�����!�K�����ۇ64^Q�:.���i�b6��a�K�3���`-�����4T	����-@����ώM_Ž�<1��w�fxj��ݬ:����B�]dS����n���q��\��1�M)�F{r�������q0��"
i�{2�Yڗ���R�M�WF�� 6	��7�'K����<�T�q	z%x�
�]�)���t"2���7��'��IJ�����>Z��!o�{�~w���h�|�o쎩VF+|���
{�YZ.��{[�L�.JN�0�=��4B�.�lr�E�8q����,��Ч'�/.������FD+��#�2N$#�d�V���z䳖&X���V�`n�<���M0\����x!~�RO]�L*d1��хU����X�)
a'0�I��grcRY̊m�"�oT��rŒ�t NM��40�:�n�2:5�By�f)�E����<�w��q�Umz�F?�û��m�~����
~d��C���2Ř6�;^&1�a����Dx���/Y�_<H�k�pu�O��'�`�!�k�F;�?Z>h��� �rt��sN{�����3�i��+p1�q���[�sE�Kjh�a[_z�=����;��/�����x�n!��6�=|����:��i߲6V�T��*8��a�u�9]��0�#�)\��~�"ݲG���ֱ�z��j�Ŧ4Z�fy`���]���&�M��3ںi[z�ݏI��i��2_I`�����Yx6[��r&H������ԙ�MF�����f5���~"I�3�F�B�u�Aa�lHa>���9E�7���4� J'�W�6=������3��H��
�'��W7~������ţ ���w���%C�A�.�4��0��	w7�|�g�Q/�(�:!��}<WDiH��wQIjCVsi%HH] i?3ђ�.8�~��`~p�ykћQ9���;)#�A:�q���*�����x�6���C�U)Pm̃�	m8�䂶B(����m�2���5:12BM)?.RP/��JaQ�I5��Qp�3uہ��<�0��d�[�\� �mgXYr���VWC��O��#Bww�؇��������c%�Us$q9+m��f��&�PjѨ��ZY�O��o�ͩ(�
����L$V����J|��P��A;�.�����ߐ�!F��̌>�[������bR�f�KT�ȤDt�(�<���7p�Q�ۮa=K�̲��{6�Ӣ��b�}v�u�*�q"#������z�X�Zʧ����?qu|ֽ��i�.C�߶4�R=��k��7t���ؓ���թ�FA,rm���vM��Be<��0e�G}M��sEu݈4��j�i�]L�I: ���F����=�M���nB��@�Ճȷ�;�o��M��oV���5�gj��40/�.D��h�t$�d����Й�ե)/w�`���q�R�W��4?I����r,�\��3�o�(�@�mH��a��Q?w��J]LD�T�o�Qpv�����so�k�Ʋip�Gd�lMqA^���:�<D�;���jM�ϕ,��Ֆv6��38�z�(����j�W,<�]t��a�Po�K��z=*sMK1�Ց)!���
��3v�,�Bsx�}��ä�\�٧B�a˪J���3�T��T1E�V��bW0 ���Պ�:�J�dx҇�2�R�C�јL.j�Y8�k���O�	D>�p˃����An���W=J(� ����*p�J4���;�G2��le��^8�qʥ3��b��թp,��@�,+��^�#��IS��S�+�m�y� ��j��v%g��I�������!�$��'��&�஗������'�f�q��F��XX~��� f��/�Mi�
:tF�qV\�i2?&5s��ڢ��]����[X����I�?�c���(����������Ȁ�Q��R�pz��<��, }ng����x��g��c�w@+k�q����L{��:+�P@JE�_3,�袋�B���6���ڔ�̽�wc�@{E��q�~ ��L|�c��tnÆD�4�T���l���/6f���&Y�~cd��W�(����|���\�����@|6S=���O|�}l�kd+����^�-+a)����R<��:��T𥄘lk��q��ڛ_�(���%�;tھ�)��|Q����fՏCv[�{y���D�VA3Y�}����c��+��xp����/ꁫRd��t�(��!-{�a�s�ט��{��!_IM+��6�Ц>�I����/y��'ݘ����
���o=���fY��愩��2�.�v�U�I��g3$��U�C��цC�[w/W�������HC�ff�f���uڑI
��碊Ӝ�`csIE��E3��U#f�`��)�5jVk����1CsO9g����781��uo\	�!`�eG�����0$'���NȰ�S�%�����I���=��$�<�O�v�m�����d����+�:&T`���@�%�;b��@����g�=汌�l�2��1~-;����c)�a��1|F }e��X��8��Ը�Y�\ �C�!��c�F��R����Z��D����9:3ASD��@��SA;c���.Y܉��nxZ�7QL% `a@�CY�~Cl��%Ӕ43���C����}$��N*-�$�䋕Ϳ�؜)�Z���Cy$gг;�� �c�G+P\I�@��J��'�҆;����ӑa&�UB�Pb��СYq>Ym�X�q.���Ϋ� ZK���*�
"�Y��1��ԛ���ԀG4���d`_���u�U��ME�	AW^�j��oγ����v��H��
"����mQ��o��"�$T��QRϥ_��y�xh^%��߹�v��Ju�86��!k�-]o4$t��FfђS5�˖�^�p�l�������i;�&Sz��v������69�F?�	����k4�j.�r�����9�����tk %.��yj�3���{�c�@?�]4*�g3t�9�*o�f�uu�&�����d�BXM��r�P�Ժ.p�B/����GȂ��f2�Wuz���������M��ΟgkK$Cz���P�<Gt�6�z���|鑸U{�VA��qTs��I��3W���`�Y�K���z&�O�H��+.w�QS�o�MM��u|�G�@k�S���K�UX����亢�4� (� I�ؖ�U2�B���M��K�xg�%����� Y�= �ï��4O�`��E�$G2�����1��P�DZ���nB��bY��>�����^*�����x_B�{NLK�i1M_@��-�C�5����^>������6k��h�'�X�)kV��иw;�W1B�&1(�^��}=� ��	��7\�#�P)�O��m�z�Z����� ��vq\��ʊ�ْ6�_oL��6%-���vv��M���4�pF���dĚ��o!�O��􋴕�[Q�J��8�>���ƣ��s���]�y2�;�>�F�-h���ܱw�?ׯ���%g�غ��C|�9�*�@ݪV�+qnHM��|�|�2���tm�&�u��q���y�k�9 ���\^ �4&ڶD�A�KT�-�AT��3�+W��3�P����(?=ҁv��g8b�k�❋��iK������X���K��S�z��S�u]OD�G�X�x0ţ@��<�:�V����h�~l�%���d�=n:�/Ak�-7�ː��g?��!��>0R���@Ӥ=1�P2����Ù��tn�>\����*�^���C���"�)U����O�_R�e�o	+�@�&�ŭ�Y�`�r���=�d��>�@�)gZL���=�E�ܿ��m\�;�5&�y2f- n�R������v���K�ȓrD%c0�h�s
03Y�}�F�[=�����lb�R�L��ͪޓ�H���6��vΊ�NܴRa�f4q8Jl�_��:�zi~n#ؗ$�Q�j@�e�B%;R�6<?3�P�*d|L܋8�G2����k�-���� �H$���37J�_�Y�������c��Dී��h�@���몴�i�顈�e&(Ko�L���ao��ڐ�m�@���wn �Xz���ꡠ��M�H(�H�<����0�J��=ͷ&��^�'�`a��AY�JU�N�О�@���q@i$� [T��i��ep��"��t(vx�����"�@\��va�Y�f�7�=�)�� ��'< ��2���,E&oY4x,&�����D�a�A�P�)�=���p>�-�ԣ���~�o��v��E��%
endstream
endobj
105 0 obj <<
/Length1 1395
/Length2 6101
/Length3 0
/Length 7050      
/Filter /FlateDecode
>>
stream
xڍVT�'�CJRp4H�	�F�6`�� ��t��H�4ҍt
�!%J+]w������s��9����{���3�����"���8�O�(	x��' ���@!Bvv(�	�����슄"������B�u� �M��9�����b�@ @����*	P�CmZ� u�$d�p�r��CP�*\6� A		1��� y�j��@(��hr�#l�`�׿RpICP(gI~ɏp������  =0�����x���'d@��?��;��@+��6`8���е�j� mg0�g�?x^@�_��tF�J���� `� �n��:��ʚ�(O/ ���rB"�� w�	d�v��8�,� ����i�
uF!��P�_~�A_����QH�_�)B]�6�[��=VG8�����
������Y�uq�)��V���� "@ PL\ v�=m ��x9������8#�vh`?��C���(W7���7�["�BmP k�=N�Ov�l�����+�`
DO ����d��-�������
��j�����IA�	����D�h� ���g��_�ku@�?{��Qn� H�}w�p��\�7��#�<����Ph�����w����_Y���g?�nNN��\����
�A�����Y�Bo����t}�c����P7�Z�P �f�����D(R�	�Ձ�l ���;A�`�����z�l�O	=��&0z��]R	n����sB"� ��+ȋ=x�$�D/�-��7��p
@���!\	��@ �X�R���uP���
ٸ�����7�]�%��t0�lC8;���
q�
i>��g��[��ľ�$���gֵ��/��<#h��D>���laM��Xn�ޕ�v�{���ݦKߟ�qzc�M�3�{Fs�嫻	���}��r�5
t�n�lSg�rq'�ɦ<��R��.�z1����\T��g�8_�a�Y`��[�ד�,�(>F��?<oO�|�x3zsO=���o'J8��dY(�|�{��@�N�FgBˈ}L14�ᣰ��N3�S���3�� �w�	o���Ыr��Kx݇J���V�/YI�N��/	�EuT��,�wʛ,_R�*����v
s�fzS���Ǚy���[)��	c��c#�����^�ɖ��FO��n����<_{���M���Ͻ#�Z���l��g�����^S�C!�||��~QHP�{k����刻����J�:���t�{�D�y!�][��~�>���Sr�m���vp���/��b�4���0}*����%Gt���Q�7z1q�V�Ķ���:۶i�~b���y%
�O�R�EN��1��w7x31҇� �?�n�n缒��KH�;.%v�'b�)�	ڛxz�忽����l^NC:b�sT�X�	>X�^plm.�-�msp
4�曅�O���?dg�q[(��ǰ)rLoH�ʔ�H���}AS|��)��'t�1�8�Ϭ������
�����*Q��P�(���gy�=��?���s���nw��y� �Kr�4vP�8�����E��Z>�l@90׳"f1�6ݝ#��,i�v�HZ.ඣg�-�J�o{� �7����l�7;��?Ԧ���k�i�ƎFc�2���撃�����-&�敏,u2��d����ȱv+�W;�y���x���US#s��I�WL�@�`��xR:�U���q�૲pY�{�)���J���:(���1�2�AH[���J�٤���e��0���ߧB�~iƘc�������C#cD�����}�m�7���)x��b�V����"nk�,���pb���<ȿ��/�:����S<��3���~��.��f﫟T���g��j˗�6��;^��X��ՙK|3�ۃtպ��إ���tk�įw���YO/4��h4��A�:�fM?%/x�H�Vu����?�9�n�"\þ���|�&�@h��>�(�`���a�W�����ᖽ���V���]������zF3�l�#�yd�)Leua0�|v�7���ɠ�]z��%�O�Q��x}�k���3������I��[(s�I�6�5ý�a�\�j�L�w��ls�?�p,�~�*��V�o�_%��&I���޺��<7���d��pI��Zkr��3�+3�;s�qX?M_��ݽ&�u%sc�fx�ٖ;T���΋l9�E^V��@r��U�N�����8o��D��=�r�k�P$��+;�oB���{f��gw��n�,Kl��
%���vX��⿎�j��$��fZ�(u��߈�9s'^�� ��4�7�dߡe�|m���2�����g��mQ��2w�A>H�����|ٜ�#�����Ł.���t/LnjN���d:gy�����3�F�DhgC\��4��rW��N,��� ���d�X�>v���I�w��� ;�G��c��aoMh����sC]h��?
��
�w�y��vO��^0�g�Q�q��#�WO���q^ت4��Ƶ� -��Ԝ��sXx�9��^���
x�?X.�����!R�PA�M���b�T7꜓r����I_ȾAO$������������V\D��źҝU�2�"��뽵^ܩ~~L#��qT��"y(��[�NQ��I����M�p��i���	ݽ�j�Z��5e*��m��8}CV��럷U�>]<��;/yF� -��9�Q��A$���ߔ���;'Vg���;��{�ò��1�:��7�����&Ð���9<��g��s�1
�0&U�mɏ�U�\%w�V5����tbZ�f���Ӛ̱��ǧ��V4`OL�F�AX| ���q<o�i�\q��j�>;��g9:�d�!��\��Эz��8�i'-r �4*�b�n���^���({�����ɧ��;�/HM�-0�
`���`�Ǳ�uJ
|�۔�E�dH`h\v�%C� ْ�+^R)�����GY=�s�IΩ̊I��z�$Pq���z��8{��dp'�3�[[侫��-'��Y��N��#��ж���X��*��%����۔�#ך�ҏ=1��+��c��<ˇ@��k�\��4N�����0� 
��;]ܚ�3)Le���RV��a�]w&'o�ݕy�;؆ϕE9{�9�7h�����YL?0����1�3CN��{Sߒ�7{�wWy��n�6&�L��������?lG~c!o+��!�N/�ɨ��)��!�4Zq�.ِ,�mN�.��[�qu�s���C.�).���*ǟ�ID���w�Q�t"�w/g!���=&o���J�f�/δ�03���gܕþ_[֋��d{���)�?t�ٻ�u�ӊ�z}d�i#0�+���iO�:�e���G��c��E:�K�T�Ӎ�	c��K��	U�ɠ�wD�T`�|Vֳ*��p� C����,��!\�����c6N���oj��i��r<H�&"�c�9l��}�TH����D( (?B�}&���K�$#���y�����~��Ls�c�����DCV�K�$-f�Iˇ��J�~�Ց-��utMJ ��[�y.m6d=�$��oH��]�����dÊ�$/��)[�t�Lx���T�n0��59[����m3�}m�d������f�z�0��� "���6.�UT��%�e$�K4Uz�e�ݼ��^���e��#a�I�M��<�]@�J��]G���y&��[�qxx�x�Ir,womW!�\���<W܄��� c��Y2c�˰�ey�͔~�
��wcPٻ�]&�'�%���Q��?�'cʶ�T�(�4k�{���ތBip;�\�Ͽ����&c�ˮ,V��.˨�����m�e5P���\:{��s[:�{$���@�S���^T���з�0����m�!�i^ɳ'i;`��Lhӵ��l=2׌�E��V;�妯��l��w2ќ���T����(�%�-�:��!sBՉٴ�[o���yA��g}��4��a��˼f�'Ԃ�r�3}hI�k4����9Vr��`�©�?���3/#P�Ǎ&�|o�փ�T��8]��]��+�I4O網e,,��V�Q`���Z�d��|<\��R���X��F8�ﳕO���Fc��,�sDq�|}	VY�r�֯"��A���;�{٫��&�8d�.1�J�5����x�y�?��8}�@��uL��]��"����(�_��0k����aA�=F�Ƅ����#�-mϷ�>暦�Q�����
X;&Se��W\�ҽ�;����OƷ�sSKԞP��z~Q����FP�=�]2�9����gG�m3�(Yv��/N�cx����ْ(GF�g�1�R/$��P��Y=�XA;̋a[[�_��:������0���|Y�KU�Qݩ� ��eU��z��d���,߀]�n܍`_��,�����4!���|�b���6��\���|A��N"ϲ%`oΉ���cP�Hw�.����1>��E}��ٞ�~
װu7��V�8t�V�sO�9��pBJ����8ͤic�JF�va7g�i֋_ߋ����L>�v9��h��ҙc� �Jug9nû�������;j���g���5���;6���p�5n�y������Ȟ:�,(�%�4� �/=� �z���T1�&���0��!�n�jY�B�j_w��u��@[��H��T��K�z�^5�yv	�ѝ8�N�Ȋ�)S����4� h�2���n�m���</V�Y���Y�߁��Ejʢ(=�X�D�9�VeVE�`��I��u���x-j�5���H��f��t&���j�]~\��V��t>̢)�H��m���Y��)���ZX�{��8��a/(����]er7�B��3)����2�B����Y������`��?TA���>�xTh�`���~Kd�T�G�~Ĵ�ݣF��ɉ8�<�235#ߩpq9kɽ7�"�)��֩9�?P�����t��9p�3��	>zA�H''���e&������K1Gp��An��9]��UgT���|cepJ� ������eq��+�1������a�J6�-�j{\����S��������!wC��ulb|��{۪���oNjR:�?rH�ƄԶ�
�c�@��3=�?(���K�Aۅ��.a��� }�[�I��,�����I�Y���R5� �^ ���\�5�p�G�G�Z�s�C�t���d�ȗc�,�dυK��9�R����H,�8	f�(\�q��?�̎|)�OY�V�Q���g��;RO�}ԉ���Ǣ.=D�j+�W�*jje��0�@�Kmy��ۢi��^��>h*���-i�on�L'|��;�~��J,_����اSbg�C
��\�.I�P���Q����*�cd!��� �����ݤ�o>�O�/o���X�������\{ӄ�1 <b���I9Bɯ���i�'(��HDo��Z�Κ��x�zm�޾t�Kٶ�s�Iy����>pm0޿�((�,f��9-:�m�'W{T��m{N�Z�|�onKfW��0�W�󖨇C˩)��'��%[�(��.7Y=Mlq��{�6�*��l$z�4��*��-D~I"M߲���nw�k����ߴ�C�D����*�Ϟ_?+ 3���^���?J��\X/�S[�ؤ@4e������3�*�7�t)3��ag/ꛊ����U��u!v��%/v.R���W1�bm3�=	��~8�Rߖ�O�}���4�^��Zz�-����{@��S�0<���\�Wj�~z�>Z�R2q�4���\�_�4���ʙ��F���=����[�GZ�o�W���q����l�^:eY�s"�sf�,�J�IG�B��ʼ�Ď���c����gWQ����y�.9S����V�	=��̑.	�~N�G�gZ{ek�A�n�y�X
�CAD@�Pˊ3o�q��ld���ɟ���T�*��`/�A1��Q�z�ޞ�?&E���n�,6^��Z��4;-��BO5�\�\~7�yٻ� ��0?��hɬ��4���'����}�-Ǖt���,��0j.Q��#I.o�4����q8^n�0�-�$n�:������ڤ�=���⃭��`�����օ��t���O��DA��v&G�]�S?<�i�%b,iDW?��q4��>e�����R��i�]��}6I�|��o�a�;v{�XӾCor��ncU?��hs��7ΟL?�\-�o�SI/y��%
�X�}�a~T.�_�1����A��;�n�$�!�B�s��z%�@�����:��U��sC[p��>��|����g�GOf*��X�5��X���*����g�n0����xg��g��R@�ͮ��/%ײc���՚A�j�[��X�]~jz�t��#���1��Q�"U,,��V��D��:kЮ���6���$��G7�%�*�����	�;M���iRʂ���b��:��k�K
�ر������"|�J��������up�-a��f2 EB���f�Ӵ:<M�jX�+��]P���Q��t����{0R�V3o��]�t��O ����la�َ����8qb{qE'\����iۗ����ۙ��U����*@����3>�`N�����M��P�r���\�D��� �C�m#Gs�!ä7'|m{�Y>i�ܝ��Q��g�̔���{�;�7������y���VNS�� �(��	`qAG���f5�����?>\2��J�+�:��J�?`hfj���nu���5EA1�-�&�y��(ͣhM%P�����&V��)�����/R��;�UF,:*|��jJ)O~�{X^��Z�y���=�0��M"e�c�asb�t���L5hQI��-���׻7��sS������%�5�w3�^w�T*�?��V�	��n�8+�m�l%���1pg��Z�k"%���y��:���s�t[Y�� ��ϸ�������?"�n���/&ZEZ+Z��Q	�q�7����/-�ŗ
endstream
endobj
107 0 obj <<
/Length1 1681
/Length2 8710
/Length3 0
/Length 9799      
/Filter /FlateDecode
>>
stream
xڍwP\�6�ŝ��^�݊�	����V\��Bq� Ž/Ž@�"7�k����̽������=3a���㔱�X!`'/�@NCW������Ç�Ȩ�9�@��P7,��h�c�V0��Puw��x��x��xx |<<�)B�b y+�-@��
ݰ� .�P���#�ņ�+**��� ���l�� +�����	��a��r�"� ���qs{zzrY9�qA��R� O��tB=���_�4������b�;����� v0O+( �@6@���l��z*� - �e�?8 ^����owZ�r�6����8�X��A`{��	�RT�y�8 V`�_�VNn���������;q+����
^ߟչ�@A.07.7�ӯ
���_��V���ܰ~�'�m�������`�'����lk��[wn0���"������ �<<<�"� �+ �e���˹���7�������q��K ����,_7+  u���'�o	��`����� 0�?��0���y(�`�<^ ϯ��'s�l�B�N����n.����������de!^ _N> '� ��W� ?�������U�oT�
�gn<�xT�A �� ������s&X�\V��#hB�s��3�f<�<6�/�����6����//����w>��NN�Y�_���Z9������S��o����V5���@[����*0+�fȀ���D��"�h���8�1@� ��	jC�@�� '/�q�]�q�?%n�N����U�wH������	
��P+o,x�� �����@��s��C`p �<������ ny����p��筡V6�@�[i�������p����oHف��7� �п^�(p�c  ���?2���l���� ���G����+�;"ܣ�dq�O�� �Gf�H0O�?��7��럐" n ��_m�q��3��^x�����@��kab#��1��^�'���4��KVN�h��.z:k]n�*�L��;�����_h�|�ۚ�#�Su:~��Z$�Nnv`}� �/ٗi�H���S��ߝ��a�#rb�*c����v1ѕ�%�����#s�:[uBjط�S��qf��3�E�y��th0N*6�/���ӄ�4���X�����&k|	׳>���|n�&�T��#�L��;�d�KW�#���]���i�N;,>���W���ϙ��X(ȹ��Ub�VەS��iS��N�TW�T��Y�qh�����Q���h�����>�_ݚl��S���/��+��Il�;���Ǽ*��K�/^�cӒ~/�Y8�����y^OXpG3���>�gڕ'�۞\A��6�V!�U�ޅ�Dj8w`k��umo1�-�5�>��ڙ�D�\�S{��#��Ų�W�a�zo������$MJ���>4���%�[~7r����#�?g���M�~��;Y��JPw�]�[� �C�`Û������t��T5�p$wV�z�躨o�Ʉa���f�/ki�-߈�ӗZ�H�y��{�n?D4�&Mp�	�ꐂ�ֱ���J���ٌT��7�7j���/@�\��Ӕe�P�Eb6�F�y�ݷ��+EX�������S�%�#���:�&�/��+�@��"~lռNR�ض��Ȉ߲0�8%��;5��%l�|	D[-(�-qM��@{�l�����x�ӞcP�&(��<'�!�,^� ��=#F�j�N�4$p$!����V}���t�f����A.��s�'�^�����	��<�;�s�%�<��]��j��� 0�J����;Ѭ����:�_H Y@���I*��X-�qd�
������Aʥ@�h�E��9��"�%�m^A|��aB������9亮`�r�C��V��.���z��^���=ϱ���=�����)s���HFS�a��:�J��ik�����Is�]�$�8����-� a��|�B��/q6[�/=�x�!����灹>Yx��h֔��SZ�I�@��Ф�@K%��[�+v�դ+���L���nn�\�Gd2C<~�sq�L��1f�m�����*��j�Cʫ?�ðrH���B BpV�?�1��R`�,��R^�p��)�Nr�(�n����� %g^w�P�Y�h����//�lG��\�������9�-�n,�`�,5�S�Ea��+T�,�}� n6Lf�^����+P�d*�^~�u�p*-�/T"����C�<�+�q�=�Zr�@�JI���v�PqE䒢V]��m�N$���G�K>c��E��K����[��87�5�T
��:�U<�w,O�$�MmY�ޮ0���.2X�_/�Dk,d�H^j2�&���t��s���@���.���d��x�d�3V�?)}\��9҈g	3�}�U�ǅ֋��΍�zN���FM��Lo�����������3�"o����CM��F�}�B�LI[R$[�̟W��f�Īi���í�����<ZE�D.��F�{h���	��!������y�kK*����f����^�g��!�Q,��<P������$a
�Œ�����z<ZU`0���6�}c$��O9��S�H��a��|+6�F�Q��w��C�@x;��啓�WI9%�	Y�ۧ�}��gC2�:"!2�~��ּ�>5/ �I�e�&f��\��G���z���X�zgee1A�y�Գ�fY��}�4�$�f��1�ii�d����ӗ�=��ܿ^b~�#��'#���3�1�x%�O�r9�*�u�O\ؗ�S��Q4����� ;�=�� CG��f�	`���ծ/�}#��'�u�����H�~�!Z��H0���v0}�N�r	��	�2G��TB�y���4$:v���gPu	stv�-�w�oD�7{��H�._H�r;�U��|�m�G��?�����1e �l��x%WD%_m���<�@��6�<b:��_?c�d�'�h��)��a�i�㢗n�y��<�C�H�e�ƱzH�[J�5,�Q�l�f�6�U߉�3��Qtz�&�1�d�눒t�ׂ�����[��E���d�4�{|�L	]��xf;p'_�>��u`때Q�_0�ĥ1G~z˵���gmt�x_8σ���<c�Im6�$e���`�@�����H}�S)j�p�"S�=��
�3f�B&�.6�f��ԯB�,u'U$g�m=�,h>�Q/�U\ʮ�_4Xu	���f��2}7ػky�9����Qg�>�=� T�47Ty�Za�wtD��P���X�����j쒋6�m� ��������hc�+�Cr�f�7_�!p��#g���kپې�L�NG^�6���ޛ�"c$΃�}����GM�uN�@���G�����^ȏP-��YN�k�788�HS�Q�4D��X���� �r]��q����ŋ���FTN#��C�U<IY��{�ʵ�@:� ���8h���cy#ʓ)�5�/���-��`}d�+C�P�\���Du p�R�*�Q�M�!�X�]�rot�4�z��^jY��|�lFT�����y�^tcY%���T���IQ�)� 5���^gF��k'�2��k����HD���O����������A�1�m.}B������H�w�I�wX�X��9�͝�U��5A��(���ʄڲ�h6����U2[-?�9�>��
{�jǜ�N�i،5�J���|A�P`P�`�]�LW�BB����ۤb 
�~�1��ݺ��G���3(��"gu=fE�}�\M��3�g%�@�����(섯�G)�<Ke[@r}.Z9��x���25���G$��ޱc��|�ˢ�C�45	�I+�ޝ�"}uf�*$�,{����\)A�ꈬ��xn)��>�	�;��6R|�F����y���5V�8*��9�o����^s`���2����Wh�Z���]�6��c-]�G�ҧ�H$w�4N���$�|��O��/�է8H��G��f��"PH����;��QM{��Py=G�1���H�����K63�
�*c��{g��l�jx��Q�kWy�uG�xP;�����eg�(�pt�AӸD�����Y`��������'��H��b�X�q�Y|����"W��d��z���P�b��Z�� ��I���9E�2;3U��Q�6Wg{�C�ýWr��_���$�����JQ;�#s]I��7�����Õ^t-ϭ_
�?Ým�J龤z)Z+�/�5S�)Gm�U�`�(����K��P�PA\]w&��J��<K�y����'���w^(��\�(�@��#��b>��ׁ<b����ǐ���u�Ŏ��֝\�O0���I!q�ORF\���n,�p����(�ٔ{Y�l:���39�~dOi�y��� ��r/���th�M��v��B�4w|cX_kk�$�Zsk^�䧆!������
�$!e�z��sd�gp����fN5#�)6H���ܸ�j��{���+�B�싻�t��w��&RB{oA'���ȷ?}�бM��ht�q�Zy���^���s�'Y��\���6[�Ac��-�X��V�ſ�W"+�5�[.�Z%mj���+�>K�U���qI�2_AQ��b�1�^b�jk'��ns_��PA,
@�(	���_tՐ+T]+�����|��:�^Y�Zt֞����H￩�!N�T�e��
�$��#{Z��%L/�4w�YV~]��� ��r~�:%�Z�֠�2;3f�~N�uY�y�n�РgJ����O���l��APŞ�4A�wW~�P�x�}��+��M2�m9s���
�i��[҂�ϒ�r3�?���5�pDI�[�+�|�׈���]�o�n��!�>�;9�VX�C��SU1oNH�[��r>��T;,3\�s`�+����S�y��ʏ��}����T�&���Cf��R.�i���a�U4-���|�g�S���J�a�n��J��ǡ����v
�3%Z�	.�����>��s���H"�̕���F����x��:by�7���k��:O��Vv�]m�ݦq[|b^v�6P<�Jt� 
vM.Hu��Ya�a�..O_�b�sKJ�_L6��O�Ԡ�f� �.�
���ޕ*R���nʴ�<6��}|C�A%�c�s#��c/�L����ꐑ�u�G>�w�� I�V�;��v��/�X��r*�i5�c�9����b����;	��g,�j�ew�=/1X��"���q�g�����j�]��4,旌���8�3!�{:�<>�2��]hD{��x;zVu+q���E���<��N�1֣Rj�ILJi�{�G.͓24����~�w��Q�uk��2tU�Y[����M���q��Ⳃ��V���]�����禽S���U�s���9��1��W#P���o?6ȡp6߷�&�<��-���(��l;�ߍȥ�������2��f{�u���
]�Ag$��ؤ�n�k�ʶ��ʠc�ט1���l�~�])h��Ey�҈��k{��"���Ts�iz!�ɛT[��Kj���|�ur�-�!?�p<����i�M���T9�G��E��E�	�V_�}ϏM�H��JRaU��d�M�0�����H�$�E��[w��� �@�%.��51�/EL�R�D��=$t�E.��]���+
ńӎ���i�U��9�D�l���
�+9�����z>v[��Q�i11�x_raҗ�k��B��=0ل}�9����2��D˿Qp��	'�V�a�Z�$7����D9�J2��G_6���$��&�����rʻ����m�Q�pP�s��MO?��>�Lzk_�m��(�bIq��ِtt����>�������y�YL�un�\��c_����f[}A��*t�9mŹ:.Bct
]�;i>M�W��� ��q�	�0�n ������}��:8X:�$4�i�K,�
$l�?�z��W#WC�p�p��2�-9v.<���T	�Ӿ�]�$Z݋w�y-�	�ca�����+$+�э��D�B�$�7A\{'�ʔb��c�|�,��we�U5GU��j��<G��A��6�h͉�T ��"61e�%��Ϧrd�x\�b��������E!��x� `�js+%��2 �9��'�́K#��1�nAo�����/S�;/�Q02��V"�.����G�B`��������R���)�ը�]�F3�ʴ���
�5[�S5j��+�ǧP
J��ӯXam& ������ԏ�?M%�����~��/���ct��2{]��y�ؔ��`؆�f������K�;�4��YE�_�Q_�o�h��׊FS|)���{[��~GdrO=�9������~����@|C��>w1�.;�q\:�<8��S�;{ܛ[N��8�Z�B��ϱ�����cg�����)�j���b���5����4@+�cI6���W1d͏�(Ľ�/Kx΄e�cVˡɳ��
�C>25;+�F�Бo��s�.&�.�C����\����̕M�;	�k�ߧD1�?S�}o%,;H�.�H�����4^^!Y�U׃uio߇ ̛�7�#uE
"]�N��7�����U��s��T��U�Pn�Ρ��	֤3Hxcw��(i?p�V&�m�z˼��o��@b�h����lK!5y�M�n����߈#ҿ�hn'�q����:�$a�����<n�$7�E^T�_A�Ƿ@�dG
RN)˒�ű�%3
���㋕�ޓӹ���*=�p�������N7!�,"!�_9i���S���Ai"�����$�5|*���3n��ě/*�i\j�3��4#5׃٥I���� �,1F{���0'TP��w�b��4��=��]!b/�1Þ����!�%�슮�Sj��w�:6����<E�^=��o��e�����(L�#�!As3_��~R�OU��	�}=����s��9]��v\&�$�$�lRcKa*�R�Jl�'�J.}����W�?�D��n0ϱ6t	�ER�{Dؤ��`�eZ4���U���F�?���Gm=�W�x�Ư(?�W�h��>�i�*w��9�O�0��HE�)�6bu\\e^z��d�׃��8[�Y1=�8/i��0ջr�tB���s�Σ�"ۥ�bE"{D��M�D$��b���f�t������g52_[��H��k�B�RSw_[�e��&<�����ۜ�?f���W�~R��3&�!���}^�]�J��N��]�x� Tps�~$�I�pBu.mM ����"D����{��:�i�������`�o�`;iEky�k��]G�l#h��)������@j�/�woP�a!���3�tD9c����}ȫ<x#��YK$rs6tKF�T�5y�+���a�"�b��w
�)�j��$�9�&.�_qb�J-0"�fU��O~E��d�g��γ|?�.��3d"q�m���Y��_�k�B�g=(�3��Ѻ�9�0�X�pe=mM��ܣ&�(�uÊ׋����jz9��`�*�X��y���ج�g�L���ԙyeg3#��)��~��ѐ�ը���(d�{��Vj!^�����سb�"���M;CZ��թ[��Ϧ�:���J-�oB�myt \`�����͓�f��(Ξ՜�`��'v��Nb@�"��-t?�i���|�֜"���V�:꒙+��i[��"h�}d?O�)�(�V��B�R��Ԛ�"	7��H��A����l��Jù�٥�«��	��
���x�Q�S��`^��1��������d��^�)�76���1��lE�K��fH�4	Lɮ��ƑF��}w�Q��G�H7�V�뤄I���P ]�贸��#��W��}jޏ+`��A��i�"��c�N���Y��y_N|�����^��n�G��+��5w"#Wf�It2���Q/�12b/���.ϗC%��=�zTe|�_��FgѼ?ٶ�ء��px������50�1wD�|?��e�C�C�	�
l�z`ُn+��yG��Ť�����t��˩M�7jP5@�:�������%���Ƒ1]/����2���x�s\=э�Xf�qf~������i2E��c�9He#T;)�&��Ҫψ�C��7��ї�ח"�פ:��T������;�͖w�7���J���r�Oi�|�5����el��C#��1"2����[��4=o�����H��z*�5?N������e�d>����w1��8��R-���q�?ϒ�� S �U+��}K�ɣW鞝���{�8�F�2��b�x�Ao��m�mo�Q�Z��&��e��p�l�9!�Eko����������r�٫!%�e���nۚ����6���gQU8�k�ŵ:9w1d���rm�%��ݕYn���o$�߼���'x��g�5>�ۖV5��Z(]�M|�2Р�z�L;����֛���{������c�ӫ�:��m���p:�ʆtl�9�n�X>�+$a�A��]����پ�h�����ŭ�G�u��P��	�l���Ħ}Ʃ�B�=;��@���������A��o<6e9�	'�(��O���n�����Ʋșr&B}¡�$�aN�������͡�-���m��G�r�v�U����O�>/�b��wn�;����`r=c��{�+F�m�vQ���Y��k�WE�+�oT��b��L�6�".���)����]A�͛�E����K�۝�E\�+:�Q����.�$*��bͺ��\������7#��ϗ͜�]Dݰmw@UE�9����$CΌ�4qVtم�h�����CN�6ϪJs̩���x<��v�+�������W�?#R�I�O_�5�s��n��~�qX��œ�쇫1����qE�=kB���I�rU�wa��;�}���ȴ��G{�*�a�{�ґ�Ċ"��oJt��C�����K�={$6ӗ���ɗ�o�鼼�v��y�t|����g���M�pt��t:�s�P�^�+���F��:�5	�"Szrz�^7B#7l�	�.�ƒ��<�e�Uޫ�d}�jЁ�ĨA���γo��S�'�	xn�L�&b_�ݜR̾=M�Qj5H��?<l���*�-El>��(~uy�]2��F1�9N|�s�����;5�w���uz�55�٫u1ZzZq�\7`�'�Y'"����쎪���$��4�ikB�6�1^�Q��lG�M\���˵��=�[�Ř 2��*;[y��N��8b���Q˗���_�^}37�t��on�l�JfTz7�H��������������'Z�]�)���d��e
3r��Ƒj��O�1v�QT5Ӓ�ML��G�캜�=O�HNw2y��h*Vقœ��j�c�Z����~�9{W�G������3LS��ڞ�b63H�%�!JAW�� ��E��c%h�<,������g��ϫ��J�PvE�V�Tf�]w�rW���(X��'Z3j�zR�[�'5۝��o�$g���߆�*���l
��M{��Ǉh �M_M��b��bV8L��d�׏�i�>c���t���ږ	�_y9����p�DǿW�d�=���;�xP�J<=���#65)�R��)�a]�"w��g�.��c��ٓ�vG2���p��1W�k�xHHr�Y�S���hq���y�����/��m�+uKy�-%����
endstream
endobj
109 0 obj <<
/Length1 1727
/Length2 9521
/Length3 0
/Length 10650     
/Filter /FlateDecode
>>
stream
xڍ�uT�m�6�tHw������%��K�K��tHKw�t
HK
�]JH(!H��������s�s_3s�\����H���)k�)A!0N^.1����(���������� s��b2�<<�P�����{��08� ���4�������+$�+,��������!P z�� �\ 5(��(u�� ;8��Y���زxEE�9�Ⱥ�<��@@s��3�] �P[0��
	G�M����Ǉ����p�b� ��a� =�'��d�-�t�!��`���և��|�  pۂ �� /�� ��W� h�� :k�����j �\�������#hkuuB���=��V�����8 @��oG��'��]�6p�?
�du@����y�z��`�\�`��
���/Yb'uuA`����S {�l����G[�!PH����`���o	v^n܆��HU�/8��/� �yxx�E�  w ��֑�7����#�o^P��`� 
ۃ�0<��  ���?�=a�����0��������<�;����������/�l�A!.~����\n9Y#%5�?�c����8�E�|�< ^~!�0�#�,���[���Wm<�2�B� �?%���o���_��
�o-(|�A ���9� �-��������7�Y�/c���Q�rq��������]�.~��S��o�&����j�s�5Av`/��mU��!qp����J`_��f��� ��8�ҁz�?3 N^��e�3�)�w��J�M������9>A! ���	o<�$��/�����psA�0x ./`����O^ ������ ������| nG����p]���{��FE�L6@[g�U��������9�4�P��!8��>���Ëw��������pß��pn�G�2x�B�[� �������� |7<���?9�)<A����+( G�{������.���'��zy�+���T���}����|A���P[��ƈ��zYJ��I�Y�]�׬���^�p��Y�r�6=�d�G��־(�\ʬ��u4�E�K�����J֛���\�D�a��H�i����@f/��=�(���I�c����N1�Ϡ�o�P��D�®�^��:�]�g�a��Њ9�"��y2:T'5:��/����,A��#�Z2;fз�� �-�ěy��>�^rr32j�K��i� ��j���ek�|;$�h
�92�8���ԁ�� m���yw
�̔wR�ڈ�<贉�:��a�� ����ֻ����2�VDg��´���1>�M�z������~,=N~��N�}kOn|Gz�J���[~|��j]�!��a���2��J�.x͂V��wd`x)f����\Y�������<R�)���r�%��8�|�m�wR��ߢ���!Kqw8o��?���yK���?4e��dN�BO�i�k�k��v���P���x��v�����$�W��"2w��&�F��= y������[`��Z�3k^[�¯oãO��gb�b3�	�P|�o�R�_�OǦԔ#y���=^xw�O�~{���W��۵�5˛z)�u�:<\v2qi��R,wﲧr�[8�hha��Xe�{��j�1M�v�`"WR�,�0����H�{�p,�nr�;)�o9���gjv:vĞ��C�����2���%��s�QY(5�b�?W%f��3 �S�k�ݥZ�)�	Z0j/,$�EOС��:�ϡ����ZK����,�?1�����x�U]�e�����jÄ�3���L<2b�n���؋�W^t�^3hh,�ӷ,,#��o�F)]ԣ���k$k�¡��譊�X'�Y퉨^�c�V�bB���/���q�&(�s��X���,C���5rX�XP�l�������wR_<��A#C��(�k-��i^gmT�OD[��R�@�-�~(Sn���ԉv��#�Z�ܟfEk$�
pf�O����#�\�W�^mɐ}=�����k�=���z�E��O��p���͘�p6p�I�&�ݶ��]�Z\�j����0�,a�5�!"�d����oCH����/Z�[3�"�"|b}�\�q(w�c���S�r)�}C���7��{%�-�E�=׆)�uo�%OJ�f��9�(������&�!;�0�~�7S5o�0N��ne(�����=�<:V�?�L���~X��۹�<طB��0Q*@����̬U:WC�M��GW��um(嘓����e���K��Z�[D<�u��Ntv��j��eKq�����V�z�5��g[]���Ɛ��'�u�T�����Y]�}��[�z�}Լ1��ܮ��G��Ű��qm m����6>n�eڸm).���}"kkg��$�M������z����}�������~?���^Z+��
_���n�Z�lz~��iGb6��M2 �,�1�_Ȅ��˛���kgi~bܠoK��U�,=�xJF�;��k)Dݡ,7��*Of���%�ס]�S�B�}�`HM�;�a7zB�G���2��`�Cz��z�7#S���*������әrHN�i�A���/�%�h�^��_�P����qU,-��s^I��K�m�|��~C���Ѫ��xn�}�c��q�!8�I;�@�g�����7t5܇�6_��H�Q�|���?��{o��P��rM�*bՁ�\��4�+�dP3XbL���M�ǔ�jD*$'�f\QQ�nV�"j���p��s���"ƺ:�+uV����0-���úl��j��%҅����3�i;G�)�gqP�i�O/�{L@�Ds!I$�O{�b�ZQ]���d�R����9��1cF�+�Y�2���(i�BC)�)b��i�>%�Y�B�+v����a�A�Ë�=�����,�^/5p_vΎ4�1#�]�0q�
����ǸD�:le)�̉��n~-'PVz	͚&���2��u GK�Q���B��r��极Z��3���c:H���.<��t�\KR����Z��5��b�c�n��2v��vIL0l� �y����k�Ŕ���Ҭ�(0.�]Xv:,H������A�ӭz�����&֥/�Ktچnف��MEj���lu��W�$D-�0,Z���^�m�n��͜>"��Y��^��[0�����1��+�S�~㭴L�d���e��Q�������ߖ���
�!����V�����z��������*9!e��� Z�O>�	��B$��ה��)<7Ӝ\��e�d�<�����	}� ���$�Z7�՗�����E��ϸ��7ț��2�H���7�:<�UB7� $�E'<����{3������ed�`�75&��7��S:���sf���MN�L�^�h���J�~�υ8>gӧ�v��Ck�
��%o A�K�Ɔ�U.�э�ma0�N�ŏ���}���\��^�l��8қekyK����K�^lNNm�xK�\��0K�b[�m�.�劯�%X��_�짮14��R�Z���{��=��4F�Q�;I�͕��ĩ�O�fg�L�V����B������
�e����jmk�C���CL�T�v/����>$:��S<]��`/ĜL��E��r��,����dT�x/��L��7����gه5]١!�Z��n��k]}f*��y���H�Ls��0�:�D�K�,�N����e�JȌj)磌�O?3��s���,W�k��Dt�!�[��5�խ����"W/��`%C�`�����{^�i����|H�sB�Ӑ-�:�X ���s�K���o �����s��4�Q�u�wS����$�JU��[�9�DegR�2�o!=R����bqe�2�V��\�g�e#����1����D�?XK�?�R�ѳy�am	ia;��Oo�hS�?�΄ʱ6E�Ӧ�ΕO>lZC�J�����T8�+%8���cj���<z�e�%SNf��Z���K�ȡ�V.�}B:u�1?<��u`����Y~��8�U H��	��EƓ2�����1�T�פQ��[��R\ąˇut�|Iq������9c������˴����%{5�,�c|k�`���\ӡS^Y	ÛŴ	�Ç��/��H����챓�6_�܂]V�}�pzD�x��I2��o�Ҿǳ�cŘo&b?~��S�9��}��(5�ޡ��Y���/�?|�*9e<_oO�y%��KW��t�B��5�lf��f�T�b&;G�E-q���6���1�����=�i��j��f��o�F9��Xsv� �q5�Paޑ��>��IF�v���7��d2��4�/'�R�ݥv�h��
�0��M=m�t{����C٨Ay�ő�cxE.o�U�2Y�5f�|�l�ňn���w'&�2zz�k�A��D��s�~l�B͚�ܛ�w��ƹ��w�w�P�]n���I�n�)�� ³C�7cS�׋�qp?�޿u-�x�2g��R�P�<�Z�ބ��M��K^#"~���L��O�6�u�|vH�q���޵i�����|�������6�kC��7゘�d�P��-?����xN"E˚�k�[��.������9�������$�#Z�����C�*����]�r7{6S��tB�^O
�pL����C�i(�G�8�6�gT���j��<�&V�t�=���R?گ?�k~m��E�Orb-�����Tꉈ���xm+14V�$�0:waQws�����C��(7�,M�(B�LR	�U��'c!������_�E��S`ʴ��x`��|����`~�8k�{�c��]+$�����cPj���&��/R��V'��Щ�J�F�e��3m�u����싘YrY�����3ܐ�A�쇏~��Zt����E�@�SU::�*��>���Q,����MKud��v����f7K�H�;���-�/)g��Ӛ��5��م��J�#a�J%0�[4����^�����F���I�]��s��ׁ�;~�e!mٔ2��ůe�y N ��V�D~J�j�8Ij���j� V9�.��O[d5�#�m��Cϐ�%c�u
Es=jR$�9�R�_J�����}<^X��h���Rк�!M�\��Sc���؝��W`�)2�|u��خ�@=�`�Q�=>,%PE��H�s�Lʧ�V#q�I��D��H��X�=�e�r��ԑA�\<��1Yw�g�:V1��i�b���G.;w4�Ү�-5ҥ�?T�!�}�.��n�k_56�q7��W)������N�ݧA�=<�H��$ x�TmdE��FU� �]F-��X	����2�O�P�<
0�)���i3=�U���r�rj� !��iK~%YӚ��g8��n]����u�kh�U�)9v�$M�����Qm\�7���:s���v���$�Un}��м=�7T��t��R��)dd9��ۄ��z����I�|@����뱵���HÚ��������4��ȇ;-��4�n��8Z�_|J �a�����nY^_V�걝��ӽ.�h�o/�����➤ԗ��.�DyF�PF�Ώ4�I�O�,���q4G�-U������t�{��)��*�}v"���W�.X�1J�~x�Z�#��6�;U�pT��.���
�:�&(}��"Y� �1`;�Ӟ9���&� �X���"/���!8K.��q�|[�g�TC�9لa��hA���	$
u�S�l�2Z"1���V9r�|.�[f�$���4�a����j����!�l����y�}]Z�n�
R]��������x��;�X���W�x�����r�=8J6ߕ��ғE.BH�!&-���)Ȳ ���d����(�A��_�'�W���I��^���L<�Jb=��ͧ��9��$V��RkW�����B�A͡�D�s'@��t�<����Tn��(1�8���=���`��~<�>���Bw1�0$QQ_�-���l��|R�;i?�w�\{�[��wjl�al���Űx9���xtJ&6]h|�����% _�u�܈�XMX�aF����:�R��&ne����k��5�x4X&�i��zV?}xY0�.�������^Ȭ�����w��Ib�̧
�0������q4ys�����9%�:���H�2Q�[6�%�󟖴Ǐ9�`�vp�]Oe����ӱ���Rf9��1K�;P�=�뺄�F���&Q���߈8ҏ(�9!dBm��ǿ�K��]+���7����e��)n�䬙�V�%���}G2���@�3�})��8.A��_ĳR�h����~�2��(y����O�Miڻ�.1j�9�����cҒ�L�-�����3��°c�wJ��ƧDQ��m�1#�5��lXo*�5�������W�I�]G .�z�����r�C��]���I����Ŧ5|���k���hT2�'Z���.R�7Z:����x�ǏŐ����p�P�lT�R&�������y6ƛE��Zen�������.��uH�,��B��f7���b��ϩ�w���!K�� $%�<�g�B�D"�,�U�p�M�M�@�%ϼ6�l�� ��Cvs�)�� �Ȟ��hj����K!T������3qխ��O����B��M�%z,�2��ͭ&.��1�zRm��E��S��:ʥ�,�j��oMk�B�'�܈"`�8@"���n��~睃�3��_�cx��P|QC�X0^�X��ь���~)��f�Y#�e0.���agE�Q*�w���E�}�K��e�{��<�A�d=��m��<qߞħ�K�´�A�Y"�3�����������B�|�ڶT�>�ٮ"2��i����i�&�Ud��/��XF�R�}_K�����KqvP(U��l���h�+�B��	y�X��F(��`�}jL�E-ڦ~5^3�k��eN��a΀W%μs�ٻ$�B�j�v+�*E��v���Fh���T�{�=�+���]r���?��n2$�fo�](����93�L�$��P	z�5��LE��.<5Q���F.�����< aز�6�V�~��!�U^,o����M㠈�lN82�W�ԎB��4𪘦(�\��Eҙ�������q�O��l�*�ȷ��c�);$$=��1�������Jq&ǆ8(�b���ӵ�*Xr��g, z��yM����V5fS�~��+��aM����f���9�I1�Ӄ�W�P���t��Ƽ�ï�Ga���,�g���6�ZHN�f�>T�<wt�>��l�X�u�!�h c����j�2`N�Ǘ=`a�򌖊@����xN�2Hu.�a�C����J8g��BS��PX���%��թ�x�GC.V������$O���$����}-79o[�0ǲ�Ȧ�'��.=�W�*������A�i����i�V��=>z)�(.�j*{��{�����LS?�k������H;V^�8�<�^�f���I�aI�!�����1_��yyj��yL�Jb��ɉ��gez��U[������"���XLjY�x|���#Հh��k��Ф����z�N|㚅��oV^�U���/V�,'�����'�Q��hׂ:�n��!���Zl�R���ۺ8I_�\J��K�ڟE�0�>D~(v����R#��}ݸe9˓T�&WEO�U��q��x7�G�9��̢��U@Ϧ�u�!� �M�C9@���ٕ<�m��@`]�8J�G�J���A�q-��I��B�]J�q�#T(8:ܒ���-�H�  u�.\y�P���'.fՐe�a���txa�M��IT�(m���n�B��9�8o'����wT8_ݣ�&�N�	�݃��Ƃ�Y�t�I��)6�W�O[ۮ:WJ����������0w�=�ٌ��oF��d���~#?��;�v"��A޳y���3g�_��<��G�ʡ��FtlG�^xDd؃��<�=H�X/�k*/b��G?���F,�2  }��ގ�oP,��3M7mi1�FWi3�2K�r�y�=/��Y6�za~�h%�
��"4�7�Eg;��ܢ?�O(���Dݍ���R��+����}�3ˤnD�ؿ;�e�lͦN\��n���.dL���:�*�4� �^���|�;�){���>!�&HpNq�+�3��Ϸd�0^��(��dÚ���ُ���m�{���v(��C�Y%�'��gq���m�+��ι��`��j!����� ���"�0M.%T��-���k[s�F҉�ϻ�E�U������O6�Z`�>�����%&��h�;N�±L����y���6.b5�5&s�HKM�n<X��Fu�R�%r��+�	f.JsU�s~4C:.B���n��޲�����y��c��T@$�2�s3Km��HK���(A~^�D��^BC&��mw�]B�l+�����"�Kwů@����U_U��Sa�b���J���烈m�P��cͩI� ��h��H�������Ip�\~��[�̵�����e��.�+�c����r6�:�fK��SFj!(v�Jܯ��ֻ$�v�O�9e����n�K��g�X?5�/�p�O>}#�nBx�E�
=5	����VG{ѶEe8P����n��9���ܬ�Gf�f-��Z�Yw�� \?���u�����+�l�)%q����KFj�Э��Jv����k�6�����F��|I�l�Z�}'�c���;�� �FZ,�G���t�ا������g��]��E��r���pHN�%Y�L�R���f�vr�~����H\f�XY���z15����b ��(9�F�<�#r����_�ȼ(Ѱڽ���� u�Tך_L7N[f���H]_�٪U��[ۏ�69��к"ӫ
��w1^]��QfA��HV���%����Cǈ�9� �ɠㆆ������}-����¢�
���/?�A��$z�
K��6O��]�
����ޞi��3�m�Jm0i�M��	j��^�H��g�l�	�Z�<�G�({���R����ñ�v�dw��ʬ�r���vBW��y���;Ǝ�aY���}d5��zO��|5��I_�g�*Rɢ��'����J�4��&�y����a�O�0#vX����>�v8�2�0�8�H$�՗�3�L�NG��o~�F����}W�����P&w(����uD��BЍ���t4�E�N��VK���>�ۗr��48E���A�
�(E4��/ֹ{�X��y�dBӕ�Ģ����%�4i�K�ͽ���Rb�~Üs��`�p�l6�S��ue=�i�5*�3��֑�x��`��#c�^`�*���za�-�%��}R6�Z�u���qU�0^� ���/�yb\*�a�:q�� ��#Vf�
1J�m�_��j�{$�#b��T�*���h}.6"��ht#a�Ѽ²����K�KӬh�(���D�!�*����k3??K8﹬��w����>88-	�3/�Ľ�/E�h�Zp�J����;G�e!��]�k�
�җ����n�@�G���$�%/	G968;N�R��ٳ@��K3� L���\�TŁ0�>�
���t���@a0����%�O 7�Z\c��|�و��BVPM ��z�{?:�w�[��GV�����g#������ĆH
��] �G���>Ш�hl&:����ed�hU(���2�v���̱���_i�f��õI-uH����2�2`��)�f���0�&{���:�*a��	�X���T䘼�\��m+Dij�`r�X��t4�F�h�GL�Z�)HI�_�"�v�<�y�\���M��E��Ϝ ��>l8Y�w���w�/vPME������>6V)������]���r�[�q�^�Q���Ch����O�ɽ��6�ao{��<i1yS��1m�Ԯΰm�YA��m�kM���2U
��v
v�4~ ��Rԧ=1ʐW�q��ٛݤ���s9S�֎��$<y%�Z^z$2�N&7�}�1�xҍϚ/��LQd�Q^X)��gwCA�/��n����_6�2}�x��v�jII�	��]_U��ڃ�g��/p��Z.nH����W�I�o/@�J+Zw^���L��F�1S�:��7���]������d��w.�V�([#�)P%�s��q"� ֆ"T��)�bt�oϭ]Gc�l��~�|(�tҲQ���0�������^��@��3GmU�|�V��D �FM�x�( �tH���,��=q�K�Ox���d�Yr�E6����0���U5��(�Ec��~i�nr���2���S����MP�je�gb�������K�k�S�&XB#	� �d����L�P���]z�Ph��{sL|;]�Wu3�H�B>��E�Ao��ԢB�e���t����tm�*lR�����}\ړҨL����դ�n�t}�d�sG�A؜����=��ż����@�_o6�ŪZ��3�B�	1����!㋪O�>�g1S�:�����a'�\=�m�>>�g�~|-�Es�
��z�L�m����1@��[�}��G��[^�m,�ՠ�GW�Â����{~���1��SF���DHR �Q��e�Kx�s��+�P-��m��8��v$�̜����9�mSP��F<""����
endstream
endobj
111 0 obj <<
/Length1 2190
/Length2 11206
/Length3 0
/Length 12508     
/Filter /FlateDecode
>>
stream
xڍ�T�6L*� ̀tw�tw70�PCw�twHww�t��ҥ(R�xιG�����o�̳������΂�RU�E�b��8��p��$�44���\���(44�`W;�?fm��� �G��3�
�I]�qJ�������+��'���dg�O �Y 	t[ �X��
����le�
=�?�� >��b� g�9��t��CO4�4 �`������vuudc���`ڻ�B��D�`Wk�:���� �"Pڃ�fƊBд��m׀X�z �A ��lrp�f�9X����r� G�����0������o��;��47��;��V K��"������ :X�
ڹ@��@w �h��s @ZL ������3��Յ�l��"ۯ2P��,$ �� W�_�I��A�Pٽ���Y[����?��`a�����#����$'�OԄ��fr𰳳�s @N ���5ۯ�^�������|!� K(	������t\��@~>:��pp ,�� 3���wu�d�7�^�3�`��= ���?A���`��;���eS�S��a���>qq�'���������`����r ���̿���_VU ����W�s�� �� �?<���V���'(C����}Cvvs�/����+��o�U�����6$�fg����/�����y� e7W�Z(A����:��WY	dv��_��+�bVv��v�{�,T������nZ�� R���=6 v���A�����@��/�O�}���9����q�����@/��C���� ϿF��� q�� ��� �g�_7
6�_���M�7 �I����lҿ/�M�7��)�F��	��s�4~#n ��o���A�t�EК��ڋ���i����7�/����_j�Y�9 l���'�}�����wr�N�,\�Ρ������	`��]a	v���/7����h��ڧ�*?��l��Ҵ�B5��(���@S�#�J�[ h2���V�����v �ߚps�cu�/����9Bw�[^n(MG;7��rB�g����}(���p@��Nၶ������������@��@���e�����Ȱ�Z;���(9W�	P����P��CNh����P�7������=s7g�4�����������|X����4�v�ԉ�x��O=�����7��J����Z�y�̼���E5�D��j���e_Ư7Ӟ��9A�d�]a��w��Y
�n��[��- �V3|鑻�g�jd��t�:��a|\�5�z\�>�%WX�i$��by=���(��]����p�2�4�f��q��]1D�^�q�㛛�9�ӹ�*��V��%��̛H!�Ȏou��繒(O���[�F�Zq6R�g�,
���>�����u���P���Dm�g�)]���Hc��J���Ru�0>�V�q��.�5��4�Q5�S��:�!k��5��-�K,"�)WPo��pTʕ�Y��c͇��R���f�T�a���b��d6c�zdH��v�,�����U�G��R�	�����S��0�����}�X�cI�r��|t1�ό�v:�z�b���[�B�6��/O/*X��íZ��bm���񰏘��)��h��ʖ��E!~�݇��{�z���T�xw��b���vj���jR��|K>UN�uv6!�95�=��Q�@j�ի� =��k����2|���3$��8���n��sʍ�@\�j�6}��_���I�#QH>NJM��<*/q�mΟZ���W�p�je�C���r��m��G��G���,��Y�h�N�����X��E�g:t�o�5QR�!�|�ոG���g.��Ka�6�h⏊�^h/J�t�u�`�k�5��n���^	�-/�e��bH�y%N� ���t]}"�eNLK��,��?Κ�F��9Ojַ�
�K��{XZ�wݒت�2���FI�p:���ҫ���pɱ)�#��!�l���%c�K���Iɸll�)_��2�J��g��a������"rW��^Kլ"�`Ph$��-��8+��&���E��Tޙ��O���G��U��,���f�;���uU����`�͸d=�k5=_�#+]�[K��	��vЛ�M�R�5k�L�^�G6ͯ�=.m,=^rN�>�p�'Մ�C�js��'Z[��b_�i��%#"��q͓'��9!6�|2��sQؖ	>h�Z�L�QZe�X_�|�q$Jw��y������0٘�|ht?A^�z�|�pg��G�
�^�akt�Ĝ�>b���.0�[��o��+�~&��$�$0ODx��ۥ�4�Ȩ"�0�i�3䡻����*3ĳs`�G
m�NƇ�OO���Pd�+��qm�JWH�U��^^��"D��f굛=�O�y�q��A�j�|bzc�������
�[\Z8kfV_�{߹��I�[�\H���)5�D	�6��Eƹd�d��Rsc�u/B
�z��gvZ��8��$�=v��l:�3K�iʕ�Ęs�-3w�[��F�b�]��.eՅ��T�`Zj6!'RL�$��78�4.�K�֢�F8�$�>�a#�LH�%�V���x�YG�{~��:����u~VB�qIM���)��7�u��D��=iU<��9���2�g"�@1\^��bH��{(WK��5ߙ�?�)�z6�� ����2���W枖"����n��&��%]d!/31 x�U29��Z)/��L�	�������}'���C^�F�w�ۨ����*���G8�&�������!ݟ5&��DB�Ls��v'���p��^��D�+���J�N��NvP,��\�ҫb�;;�)����X���������Awj�i�Gk�
��x�rR�h
���Ԑ$2BE1�#\�V��yww1
4Y���Ĉ�=���EP�§�}ާ_�e5�-��>�Ŭ��߆�� �#�3�c��������Rɠ��2�9�;^�).z��0��Q���To�����6f:j2?�8���L�=�b*���W,X:��K�O�F���U�İ�L1�hD�����
����X��X���;�)�X��g,Fj
Ǖx�Q2���!�[��O&�F���~i������������:A�n����6�YH��3EB��R-�-d,�����a�1��,�@��ա�����@�I������(q'W��r=K���g�$>�X�__�S�&�O�	���s����*%��q|�C�{������I����q�q��x�-�a��Է7��V>klD�� f%l�a�㽶�|�"�m�����5��ܽ��gC'�xI��x����mm>|�6���V�2���K�<a��JҪ�6�kY�.�q��M�̇OI�yn/��i8�M�o(NGߊa�N�(Y�:
�ƒ���-Ŝ�D5�Tn8M�֩�Ş�$9"|Q6��t�xPnfB!�v�{��c~�;=���e�4����E���9e~����`6h��R��]�qp�]sv�ok�Y�����X4�Q@��	�Ю��+�:Sn�Kj�3��x��>U�W�p�?�h�%�iX�Qh�)[���Qz��Ef�rWƬO�M����Je�E�Q0kvvp��>Z�Yk���4l�����	���o�;�����Qf�<'J,��bY��"�'NX`�����b�gʓk`�˕^��H�����B�Hfg�84� j.fx�T3kHw}|T�3LJ�шz;�vȕ�d_`�ff�\
�d[&��S�%���RLәA_9��j՟��(Bҗ�)?`�Wd �L�K��6`;�c=]??/�����4�K�5&}�,��4"��&͆9��n(�?bq�a�x�Gb߉�':rN%����\ A�]�B\`< �0�k��XvV���sV���G��' ���O�*�Վ�=IXO|n 4񠈨��3f��� N0<���� �1�b�ʳ�\�8���f���H����~�:"�|�3�k~�B��ڋ�<ڝ��9���P#(I
�y������Ѱj����i�~sk
��yzҦFw�+5zV+���eV�o�-�$�H�4�r�  V)�&�V6բ�Lt%Ee��{�)�t�О�)_4ĔMf��(����A߀:^������(���48f�1���K@�V�'���'��x�����T��z��CȚQ֕V���)A���h��=�Í�.Y���tS\�˜�Bq059a��FMQ����\��,��.���~v� S��i����
���JN�|r��V"�����F[����m� �/�[�[�&�"K�2O�X0y���o�{��(��UH�KmN7&H�޼�,L�E�O�I3;����ƻV�)�����6�D��j�lpǯ�p�Wa����8��g���kh_��>�tJ>N�Z�ލ�8{��!��n��l.P���z� o�i�Bq�M�j�>��^���t�5��yՊ`E 0C4$�4˛����'�Q�1Rn�ZHBH�D�M����znz5���62��A������rVp���!�����J���#�TV){�8�
��o�eQx7<%���ƖI�+�N�˵w	6�[��fa�B�盚嵜����I}�g�2�,�-�Xsḩ��2��}C�ᤑ�$��	�U��\��_3c�xI���'�p��Q4(�W~$P�<�e��}��y�wע������T�Vfw�eJ雥 ��7����Z�_�����,O:gѿ�i�V0�4iu�o �Ҥ�x��)��IT!^��6���V������sk�u�[�u�s%M	+���G>o ��C�0�`����<B�&K����j��c4��n��s�<�[��/;�T�V�����xn�e��N���Ī���ݛ���_p?���0lӠr��l�5��1��r��H�!�1V���ޅ�����w�d�|őxk2���isџ4�r�g��E�����I4�F��4����]���������%��E��^��pn�7p��ȓ(�=��>�|�drXT?�nu�j%�MW��Zk�b�3̞@�J�}�Y):��m���*���:��L��A:�������nO V���F��8��5�&�-ǩ��2�W\��&�iK����Ԙ�ZG�5��
�����e�����h09�E �&
fr0��+� ��3#N�W:j	��A���ʂ�g���y+UH<p�sI��D�Ӓ�O��ⳤk[7%��m!Uf�����	|M^��my�Y)�4����|�x4v0�8;�UO;x4V���K�: }�6�?���t��1�/�r��@Z�A��� D��a��[�y�L�G�����;��Q�gA���0@�����a�2C��	����+��}��̎]��ZF�v�ձ�u���%~]�O��M�z>=������|� �,hh��0<��b鰤���YPozz���F�s�����s�����T�b�'��������N�\�u��o�ł3��<�-��w�+G�G[�>!]mMąш��ؒS���D� 9�R��Nn�p#��r_���2|�uq�o�4�~�)�e����Ŕ�W��2�t���{N�����b9ѹG*8�'�'��fF�*T3���MAg��_�_��G�	�S�5�2,���轻Z�w;�QYl�^4��N�&��^�;�鶢`J�O6�>K_+Y�/���q]%��۸>���+E�����
,&���iq�hb�:�.b�.�v�U�ɰ�1�c�;�6��K%V�����\��D�8M2O+�/A�I{T��N��ir&��	�֦8��.ӷƓ�����%���$��y6����
�x�#?E��fJ�mި`��G��O�`���˃��z�&�
���P����
}���2M��Kx�Ϋ�3��F��IrL���b���A�꾽�Us�OEk�oI����D�|_�O�.}��\�E1��V�����Kc�Ie�BB�	@^�E�f#3�=H|�n>��ǂV���/��9�}��S8�:rUn��@O=���cm�֮W�MCG�z�N��R|x=ԝ>���Tо�̊{Q�Zu|-��u���ۦy���r�s�� ���'�[d�x=w��I�P�f��-֘E����C��*�h-��j���bm���aP"�ߵPJH���'���=����7�Amt��y�R�ZVG�[�?��_��BvG��<T���$VRS<�uMb�:~�������֔[�\^�.d���a�(�lsL�?���H���%kL�� +&n���?���KBF _�Ev����[Iu�30��D�����qK���+�x�6�N��s&��@�Ѷ8ы�pϋ�d�U�g�'?�𱈖U�5��w(���^�S�eXL ��n�7g|W�P�S�<�p��⊕�¬J��KI��.�I��D�����!�,_e;s����K�������C ��'������ڙ|U;6�FN)�2��{!�L�y����`�8dp���p$^��t��f�62V�:\���a���F�ZMFh2�[���.�CxH~ɛ���l9�3�b�L��N��s��ZG)�,�9�,�Vx5]�E9 ciWa����t���ֱkA/��|9sYC�YK�!�bQQ7���I"�	��ljj�m_"Y�;&��� ��n����G!%#/�(0��B5��A���U�u���%�GG��Znhi�*k�����3Q��:��$^!.����\~1�U͊���+��C�>q�x�Ͱ�~��Ճ�H~�˹v���;���$t����m�����7��Fm��&�Yvʳ�2�!'of����'$L�0j�~fD(A�;4�ۈ�3�2w���l�~�cL�d������#��d�5��C���#<:\�z�����H���NcfF��5\��"�y�N�ĵ�d�D������'%z;w'�&9<�(��<jW���w�>�����	���d�n����oX�<��\ߺ- 3s��^r��½��/���$݈4�qLE�/�`������	��Q~�� �pw\�t�^���8�{>K#�.��x_݄cU��U��%O�\W��s��H �}�%�x�P�t۴�c2B��m.�
"j[�4�@��l�d�F��=gF� �pT��3#��"�r)oȍ<8�ln�RRk>���K�Lc�`���+��D�WǾ��I�\������w�MWϰ�T�/%4��΅��l%�٫lh�Ưo�����߰$6�$s�q�趼V�C�Jk&	��<�ŭ{t����Iީ��"�������4?L�Ŗ�лZbmm��_"�T�oP�	��vۛ��=��v����R���KSgm{����hiq76��ִd��7'�DG�V�Eq�th��${2��᥷���5}�Ca��f�����l�p�ٟ'�ce���q�9�Z��u^D�	opuq#��rPQ+����蘶x�xI��G����F!��)Z�Ʉϡ�|��'T��������3��?�٘�uN�hD'��kf�j����a���Lp�Qb��;�	��y%H���G����4���Ο��.�/T��9n(�"�?1����p��$��>Dz=��*�<ݞ�3�u��
\�׊N�� ����<W	�wy%�K�Su|��\S��:�����-�Q��x����	h�O��#��fbL�� ����,h�����/[B�Y�tU�̸��gMoޱ?����X�z
�����O��
�4�D���,o�v�����؇�qL飪S�H;!�"�y�s��?�(Y�t�^��KoH4>�\2���p��5�l���%=�!�m2���~Ts�$�1�h	G�������E�Z>��߹о&Ee��,�^��M�Y\ܫʊ���>��Ce-J~)��Q�EJ�a����3
{��i��8�✵��Mv4��o�p��ץ��g߿SGkjºٰ�/�Vǝ5]dO1���ᔟ�yw�Y$ʔE�����
/�p��?3/%��=�\���\%�~=�?�"��F�ΐ�**C���<��ދYi!�2a&�ê4���3[#��n*Al���!vJ&^���΂;+2��3>�fd`o���5v������A�s7Z�����j��������z
��3WC�H�^� q��C�]�̈́���g$����_hw��i��T�u����c0�bgtѺ��Е�L�K���$->z����H/dX��x�lz�l��odcħ�	�k�s1\D/����|¬UK�!"����B�ZN�m��7�ODK �}5ƪCgֈ�Z�_�B}&tW�X��˰T��!t�M a��ઽ�����&&�����1�>Tj�Qugv��nFǥ�MT��`a]���=�SX/��5�B���ϺFU��I4$�|�{4�|���^���-Q�z��fA��(�)_;g	�J��ś~�\v?l�s~ț}��3�H"�����F=v�I�ʈu�M���<�H�q����d�\3Bo�5&]$��Kz�YE�x�Qsw1Oa�3�o58�X���c��������%N���i�®DM)�����������^-c�e締B��C4��2�-e<L��բb޴q�qz"��chi�{U�ѽ��`Z��=9y�Ŭ��lqΨ#�yt��I[V���`�����q�����q������i�yc�L!�)bԝ�[[ȋ��R6yRgg߫ 1w�Ͽ��"'T88ҘE>	UQ.99��珆~�+x��A-9=�I;�wgT�g�ӡ��]6P�r1C�m�&���H��Sλ%�]?�vX����J�}V���7�v�W,���αE���&����Az�H���OQGlb�i��/`�6�/o�w��:�C"��y�#�v�a�<�-�R(�ȋ�B����1f�ȟ�0�_6�N���ȼ��E{�r����X�ȧ�r�as�& ��6&�CܜDݪ-u��01R�xI��K'l���d4�ߞ5�ZZ�����4~�R�q~Ee��w�c\ًXB�¼J~�S5K�����f�L�e�������6^�[�-ʧ���x5VԘ]��-�N	V��ʒ�d�b.���~�gN	R9�E{i��]�(������E?z�׾�c�:��˺-��2!Nv�Fc�{Z�M��t�
-�� ���[���'��>�������1���[x��9��-z��=��r��u��e�&��+|���m&kפrd�|��6����[�K�>�#��������^��1�&���o��J�9Q'7��͉Ы�b=0՝���ztl�ka����q��L9��~1Z�����XS��c@iuG�]��1s'�3����օLo���j"P�lC#=��w��;{^��qo��Lu�~�S$�ܩ��4��LP�/����@��H`�u193�v?06��忴����W�����`lb��G�9��f���ە}�S�QUB�����/O�A��[�w���h��P�Lp��� g����a�d�;�<���rv�G׾��ű�:���L��"�/&.�)>}i��^�'*��q}.���Eg���5۸��������KVPsۄ�ٻ3l�����6��q`��o�O������v:�ގ&D.�h䄎�J��m�t�x\�^�f]�ٲ�l<x��k_�t�n�-�8�~ܔГd�<�ǊI��6C�U�����Uەk!�n��9�z9�%Pd��l$�Hi�O��)�*�v����:%�e��Ĥ�����']���T�c���v�s<{����:�$7a���IuR��p���Y�hJ�A#�(����"�G�<����uj`z�KF��}��%x����o\�p��*�j�����-PTj���,A?9�)"��������~�VӔF��%�ѽ#��u��n|��c����b_צ�u���tc����%Jƛ��r��b��"ś���
^�����o��k����=6�mG[LG��(�5�Zx��򑈝����,ڔ�SK]h��")�I�5�f�5�~��q`�)��	���\5X�[����x*�<Ki�
>z��r)0�g�x|����,ri�V$�2m�J����W�U�fԭ��b����x��NN��)�~lYO<j��PE>���I���x�l�1h��d�$�aU�9]e55�N�{}\I~`�Up��x�M-i�|��XN*Z���I��Q�T��#&����6��2Bh��7�L#&��ۙ�zda�Im�\�%���n;gQ6�3���t:�6�+w�|�Ȉë.��d�0�E�
���\�Eg�Q#w`�"��o�{�'�QW�/l��4����|��`}�k6M�n6�H��m�b�s��k�L�Vk`��|:N�7�Ų�7l�/�U�XS\�\#��)F�U�n&Ki�
�&�fc��yW��������(��6��etc.�O]�"%�mO�4���p�g3KZ��5VŶxHT��MMoϚw��Jmm�}��O�x�J����Y���CO��Ђ5�.��g��2J
�4�oDK���sWNX�>��7����(ܢ�	�����t�������7���8��H48JT�7��0�Rn�$�!�o~�0^Z�ƽ�����J��8H7�=S�L{�����F��;&�)����IY��u�}�ho�(ޡR���{�D
ߡ�Z��� P�y]	�uC4���p7d�'"^q��n�x�8\�y���}���M��d���	��=	�o_�}z����w�|��������g�8�4O����86374��&�v�i�c"�3n�G������c��Nb/��^k%z$�������o/�v]�ǽ�>�[���hQ�gdJ��m�݈LDsoa�7�8TR�Cޚ�*m�(���3R�@�4��Ɉ�^��b��>�̾U~�*���5���
��[(��J��H��2#6�-��<�Vs�����7�e��B9�3ڸ��C��O��R�ӊ³�����&���Y܀��<Ҍ�Cb��.��oz{ux
(m��m���-�����^?�>p�͈��h� 9;�w��ؒ���o'�ه��D�F�j^�~�Q,+d��}�o��R��o��aé2������o�ac�J%�ǽӄ�t ʰ����Z�"�k;X��%����D8��f�<�.��	
I�x�~d!f�)��+�!������9\u?�O�>�Nav�Mc�<��̛�f�[0��t+5a�,駿�r$�ÍJz(�Y����[R�x+�K� ����4:�Ò��X�G�%HWlZ��7�,b�.�jޕ�K�N�u�ޟ�@�ϟZ:^D6��~���B���ȸ9���4ĉ���tūd�>����,H3��h��pe��p]��j
M��ͅb��D�b8��V���c�;UX7^.ٽ���T���
Qy���g��������8ۓj�t�di�g���#�.��,��D�>WÍ����������`�F�xe��:�s��Z��2��]�
�U�).�/M���5���̏m�M��Q�,S놊t��cB�U$s�ISff�Pw5�H�l�ŷ,���QC������:���`��g����i��+����#d���E�٪�v���K¶S��t{�]q���`�j{���'����o #�=����%ǋ8Ƞ�p:�ؿx�Ct����L�p~HO�=zs	�#����Q���%�
��Wq�^��y^����{ �^��������|k����H=��cǙڠ^XQ�ೱ�U���� f=)l��M�b����	��&w��������$�"��q
c-ⳅ(I{5ƾ������4Ӳ�g��/s�?��3� (`]���-�h^^�<��������eU��� �R�}��Ӛ�q�d�� �H/�Bk��oZ�����>������Y�I�:��w��0{��ّ[�/+P�������$�o��4���u|��e�]��݊���w�>/ŵX!�]�iu9�J�V��2v���Щ ��QY���Ȟ�zT���g��G\�oi��)��ϓ2⪼z�U��K�{ݡ��:�������x��X��GԽ���g�'��R
h�(���Ng�X�f_&�z��5���u�%�2��4%�Ir� ��dOꂵ)@u��u�_2 ?���g��fs�çO(9����\!F� �J�����z��AXǵ	�����J���0�wX�Ƌ-�k���#r�\�g�(���C�|�!��J��-��Bޗ@޽�ݷ1j���P`�ɢ����mU���"#H���K�#U�����ݡ^�����O������dED �
�D$8L+s�j���+|���S�m��M��Ev�e�oL}+�>��𥹓v]����o+���~K,�ʇ�e�� ����
endstream
endobj
113 0 obj <<
/Length1 1812
/Length2 9106
/Length3 0
/Length 10216     
/Filter /FlateDecode
>>
stream
xڍ�TZ�M#!�9(-0t#� �-� C0t(���� *�ҝ�t�tH���>��<߷���Y����������}LڜR�0�<�ѕ���[ ���-��������fbҁ��C�6c3�A\�P���2.�+�&rEĩ����<| AQ!Qnn /7��a.� Y�;���xs����d`N^.PkW�6��	`�xDD�8~/H9@\�`�#@�jq@���a`(���R��۸�:��\ 8���)��jЂ�!.�K���j �_�qa3tl����0+W��0�C�G8b���%������Pw�8���W ���p�����տA/��0'����`�� ��U�\=]9  G�_� {8���ڃ,��� �R� ���˃�]�N�p.8��W��_i�,�h)sp�8�±铅�@��c��u�v�0G���
�hi��K7'��#���$�w����
�����@�O��Wz/'�o'�/3�?'��
Q�jA|a��A�������O�6�
vX@�����fG�!V1��]�� cnD�� �}��e�h/K���׿����\G_O��_�㓖�y|8� ��� n^!�� 7����s �)��U�[����` ��j@��p��-X�6��C�2��o�pp�x���������������+H�����������9@���@���+b,Ta��p��P}�_��
���9��W��)Gk��
��zB,5��`��z�?��Hou�h���_������|���!8�~� �y��-��0�_��+  �����W� �bB-!��[ �r��"� ���`.ؿnTP  ��e��@�HA*��0 ��	s��? �����	���_ ��Я���  �y@��A����� B���(��`D-� B����h�"D:��<�� B$�D�r�2\�@������"d���� B���#dx���� ����i�=�����~�!O��4,l[�tY)E���>����o�.��<��c���Fa�Y�ܲ�b Uc�8tlڗ���r�Ӥ=3`!E1�I�{U���]��I�5��Fe="��J�#A���͘%I�&D��o�|#��i$�p �0�P��He��D20.��������(�ܢDAc/���)��V�c����{3�t���W�,%�$�~ΐ5��CI����#�YZ�\�8���d'R�@?�����)eU-�����9YQ�<C�,Y�lL���厴�n�de#R(Z���ӃrDPQ���Z��|�]F{i����k#� ��ZgWg�C�yH[�PI��Rk����#?����p��P�����5�`�i��;A�Ac�hy`�a�A/�A�}�H�N}P�#��P�>�jG��L�tX%�h�)%���尐S���*���IeC�2��ZQ�"M�ΫΔqr����/#x_���m��i�E��`�Ih�#��������0��~���A�1/1y��_f����.8��5'��1�mez��J��V�PE�	�	R�m��| �a�+8�	���=����.�0G�K+���Ǌ�!s���n�}�7�� ������B�E��	��l:���eYZId��2�]�k��IӬ�MkRh���n�eosB�2)~��d���_��⤗�w���h�|�,-�}y���Ks�j\��;��3�4�W�DYĺ�k���E�y{K�n �9��Z��n6�c&������P�����`=<d��� l��:D\X ��V�Ԟ?Z��x���}�L��q���8얨���ԋ�uD��A^�s(1�bBq�$�:� 렯��� ��9�~��k�.�ģ��iB�S��2ZR��Umb�K1�h4�vդ��_jwƱ�	bl��t��2��N��H������>&��kTD̳'��5�/Uf�Ti[?�B�yw��\v[�n@��O�������;��f��S��]C�{.%��?��3�H�;��緔�|l,R%]������˄�e��8h;g잖��#�����ƚ1`�E2U��^�E�δ��\2���v��3�K"�������L�|W�u#p,��^'�����(���+ن�F,޷`�k>��'�]_�B�,�v�����S�]��6���L�D�����?W�l��=1T���+;�sw6L
��w�G$�o���5at�0qv�s��t�92C���p������w����vP�������6L��G�+T��@�$�v�$,�{��"����w��_�Ph�n��=����p�z�i���0��/�}�H�$�?�G|A��DN�Dy�)>����S�W�u1����.�'5��F�-c���^��ד� �䜖�+��y�O�_�5]���e9KA��I�+7�����I���*����lZ���%��(��N�Q��	����#ӅVҔ�y�v�F����V�6�߷��(�+V�ʾ7�k 7������]z�+=�ٴ��E���نB����b=�����3���pa�����l!�\ʒ���W�o\X�h
�Gj5ST����c�|9�G'W�TL�a^���J̟co�@��Ȑ��O���:TQ\����~]d�=�'�d5�\ �;�~RR�X�f烗eh�Gf׆3;q�j� �ZM�����=�o�"�M�cT�%h�ͫα
����� ��وR�~��������R��^��.��_�v
�Z��y*#��+��6n.���"�"�,�D�whWiSߜ�����ykA�r��Źs��zfx�_���yިJF�8��^��P�h�j�'����%$V�כ�D��{�'<����*�ۊ
>�Z�;� �+N�Ei�?��Ha�0�nkA�o"܅P�S��8��m�ĬRn�4�'�z:�u�}�{�~G�gK���v-���4�t	�%�82�9�.��nd�X��/k.=��r����H�`n����b� ��r���[ԃ����[�(AB#%?�*���2�,R��1�f��H�l��؋���������m$�c�M%J$K_��\�B��e\d��nS+��N~�a<*Aё��e�s��6���c�y4\��am�c���y�uƼ� ��w��Zp��_`vK!\�Y{�E4^ �K2�����O�,�κ6������|,ګRv��\�������X2ЏKL�lZ<W qxD�Ԁ�ު��~�zIc�y
۲is������ϸ��.{-Y�=:��ޏ0�v&�7���I�SW��L+�Jj�����E(<1�˙m�x�f���E�q]~S2*U?w6�7��k���֫u�+�z:Ԝۚ'�d���}�u�F�o:���=�H�rp9��!��D�C��������%���e���+���M�:�Տ#�Bƅ�Դi��]_
e�s�q��P�.ר(KBK�s�-C���������P	�]�@K�{ш����J>v2q���i2"��7�Rs�W9�X��[-n��$U�WG\"{���3ٻ�tƧx�G�S 쎗��?M*��?M��������E&ƴ���ֈn\(����U�\ab�l�'{��r��9�=G�a���&뗚蜭zc����\tS=u^:D��NM�ͧ<[4����z�mC��<�cx"+�|�3��~�e�z��͆���i�&�ƪ�%�q��'J���E[�h;�9���Pa�hせ��*��@M��;�sM	d��W�-:��<�J$�a�3%�\P2�m�^��9bd�9���ۧ4��g7�7 �Gv��,�l)#L��&�D�o�iK?kV4��4��ɲ��i��oI��A��J�Fs�sD��2�%Υ���dI�c�1f�P��q���m[1%NR�8]��\�����]�e�qу�!=��sHT��`E�F3�����_W�ܺ�`:��\�
�M���v�=w�UL��qf���⁗�#g_5	��#l�� ��b��*4��XnR/$��Gwx��%�6�%6���Gpv�Į��@����[ �������-1-�I�����_�����G:g�^�ڱ�;L3�>~�\B��@i/ù":Tū0�������S�St��ʯ?��e��=�O$����n0*tQ�p?���R�;�a�T���g���]���:���k�H�[�6���wF�N�2��L�1Fi�%5ntd(ZC��tb�4��F��f\��3SlgdCu�����I�D���G�1~��<����w����=᥄R-���jܑ����.Ӛz�P�X��w/�>/�GUr0K~��ìl�������O�+�p� 6�/2� �Μ���)��0�����6)�ݥ]̷78Pr:����?��{nc���s4�.��)gj�U�F�8��m���x�=TF;&>�O�"N�#s��C��;�ћY9N�b��Eړ��"[��ú�D�m�5�i��QsǅmC��d���g�eu�>���f�uaԕ:T����&�����Vc������H��N��Hr���yT�S\��՚瞔��l��5����?��tп t�Ul�6J���=�Q����V�f���K�� �%]���[X�s�Z��4�c�ݢ��*P�O!�p�7�HW�؋�f�i$b�b�8DG���T��+,��`k�S}�y�����gy����hP�
�����'v��Z��"������$��7�ȷ�F��s�b
 f��nB�����G����a}��+��%!!Y}��k~�!����Rq�r�)O;urPqO���Q�f	�Q�]��Y/�p<kƊi9Y�O&�_I�y�'�a�@�4ꠡV�-�ಜS_{�j����X8uВ�ǩ/��\)I�o�[�|�&�6]���X�5�߹�-/Nc��'c��j!�4W��B�P�#���%��=��9�T������W;;��`�3�>d���T�:jwA��t�@�Sd�g]*������8I?")��'�tt`���k�E�O*�v}X��W�g��H����r���c~��m��=FS��<�R~�h���i�&�i�6�1f_��HGQ��65�v$�!���O�@����G7������{���>k��8z{�ͬ��11Y����B�C<����3^2����#a�Ꮩ�����Kc���XœL��u��\ƈ�j�1���/��&M@���f���6����׌�l�)҃�#>�l�-Q�h�8a��|EB�}�I�H�BD��~�6�ƨ��/t��B��Y�n�z�K�x��"ϑ�����mi�==߇����L��VXf�c|�uwZ$k��@l�yo���gň/Sx=
�nqf5v�\E�s��ޙ�y?4�8Z+w��$4@{GV��@����8�e��V���������e��EZ١£wҽyk}#�%�'q�P��s5����W?�m�j��R�K>y�(X$�o�&�^�L���P&9>^�����0d_�%�[&�}V߶x���
��������_<H�O.д�n:��g�C3� t#�*U  sk��+o����������*���Q�j�G���Y�.i��X�ʒ��֌�,_D�����u�̧�Q�f�*�C�-�ns���/
�>����з���t����P?�U���H��1�w��������N��j��b�'����9j�d=e1L��v�z5��gC,!֮��DK�{wg	m@n�����%����֧�y̾ޟ��;�<�wг<s�H�d�>NE|��!_k�{�ܞku�jg>{44`H�k�v��4i�Q~1-�*~@Ǿ� 3��K�n�T�n+5���E��{5_�Be��b��E��K˅��6����1ڭ8��	�l�+L�~L����@��E�_��f\h:���ԻO�q=�C�Q��g��3�S�~����d@o	�\*x�a���Nes�k�x㮉����Ʈ�W��l��*$��_%l%PGdkb�Q(�e�C|��9|�d���:��|��� L�`j�G�+�O��nAt��Fan��k�Gf���:̋ђzre�f����g�p�"H=�g;�x��njVZ�y����Ah���-��d����#�d���6�$���ɷZ�)
��X�#!`�7�����x�nF�D!g�݆�������з�5Y�G;�Z1���&w�z3Nj�
e�pc�s۾�qk>þ���V6�����M�?qA�o7
�fB���.�.�2��Z���`Pf�ÂL���׈&�X�}����C�|��#.W=�n-u���|��H��o{z7�I��~��ۯ)%�PS��[pJ�ʳ�C��e�'7ږ=�1
��u���[&@��W�W��2Ek��v�:���֋�w�Fl�L[2�O��nΓ�K�.$h��֟v<i^s��Z<C#@��/�B�qx�[?0d�.��]/�l4�b��ԗ��\�H⇽&�D>��U�z�xX��2E?��|����?;�U����z�I�/����G
�Ø�m��&)
��t]�?�õd�0:~EJsee�9x��)�a�����JpQl]o�Q����� O�ʾ}���`�#P��P��4�>��҂$�:�F�K�Ě9��Iǎ�E�[����Z7�ef��E�Y��'�~����{o9^��h���l���04�Q`b��'�%�QI������G�hf���F,G�d��S�[�l�9�u,-�2��Eǌ��Ʌ]R�2'�N��7T�	��L�u�4���rY2��V!��/��|���ڮxEl�7.��Q|ɕAB`�j�����He@N��i]+�L�H�9"J��� ���}��W�d��^�夦sFFw�˩	ŋ��E�`�%��v��#�ث%�oH�����C�q�qq�̉ʽ z1�Q��q���$>dA�^y�t��@��/���x���Uq^0v���EʔU��l�kR)D� � 5��'\��d�+@Y�4I�>W(��t�������Ꚇ2�\�qΐLk��iG4���LT&���f�1��C�0q���P���Q�7M��n�Y�W%���+Y�-�y��,4��|n�(�|����TI�tmoGz�=����4"�]���3vڡ`Vt��a�|�O���2u��"�F�-�3�X����I�~w�y�f���"b]��M,[V�oQU��~˚lS,���?Y�N:7��ߕ���$S\:�Ԭ�m~���N�+%�ۣ�OȽ�~�9+k���O��Zn����S���^uu��S4;h
<�������4(�h��O�� �묆���:�q�O
�Q�5�&��}y�����/B���#}�W,���=��>�&�)k������������ܠ�Ӂ�~
T���Ғ����+��yM���O�_�kMfW	H!r�W~�}�疯ohCӕPˊ�xF�,喑��@7W�4�.�H(U�nU��F,��1FfK��n't3�;F��o��D��M��v����E�}��>��t�����d���+��H�z�ʄ`�)��$`;Χ8aڅ���l^?�H��T���,���U=1x����E�P�2O�/��9㑵��l��1��m��s�*'���_y�|���B�T�AS�<���ͺ=G>J��8�졮'm�j���@���UE6���aD]��r~�.�Xf'���5�>�F�Ft���k���ow�_	#�3s1I&d����w/j�|?�[Q|��D���\<�)�M�U|�Y�t���(R�}�]Qy�ؔ�h�{�7��UUK��
��C��Հ����z����	�K��eV��M��lJ���Q+�}����Աe��\��A��Bכ��\Ѐ���fSuS�sK���:�,m��Vn&�ɦ?����3?�_���!�iҢ���	7��Yz�O_�P6}Sj1�W2�v��ϫ�?D3¾��Po��i�N�t�g�nG�)`�m��H��`�jQ0�E��˨�{,��he����e��M��,o�Vu�����F�o��x�4Ǎ�L�&kO��^�������͏YQ��8�'��������h=;��]�h�1@���ʧ��`@C*��d���j��D�w^"OC'@��on�%ౢ�d�l53-���k�H�DNq�;D�+��7�~�ES��B�����K���j �$r����<�RH����yM����]S���1�x�� ���Թ���s;gC�W���M����¥��/�@�Ӂ�Nےc�bxd���欸>g{��êK���*�Fw_�LK{��Z4�y|3�&���B��0��`�E�|Ǧ�z$Q�&*y�O�C0z�T�q�k��6n����ޤ	�\w���������y���=���O;V�G|�<���%��a��v�{���)z��'l0l����6>+���M����f�r��YT�z�*X;��������O+����O��e^�-��(����9r�I;���k�7��h��n[��kܜ<y*煬$��h��z�|��!(��:]���Y\cY�ַ���4u���vBRg6;I���<�����ȯ�I�_d��-�~��Q��s�J�T-��A�4��Z�Uw�Q�m��2���p���u2��?��atdTv9N����H��P��&��j|���
V_墫���,��Xi�~� �B� ��ݮS�G8�#�8Q��ݡ����A����_U�i��Z�]�縬(�pk3���&r�>��[M0^�@�i���Ϝ�	���Nl�M�j Z{����9��1r��ԴVG�S�^�8���]�gd5�1LH?m&��i2��|h��[��� ��{ws�Іy�JRB]� w�M�B_[��h*�]�?X���!?���v	���-�}�V-��4��,I�k�}r���'�>0�W�ҝ\�u� sv�G��i�9��ZB�k��Q�,����b=�IK�p�����b���"�̦�[�+k��I�T���+9�ٰZ�ko,Y�!6��f��c�ʯ��� �0�����t}��%A}z��q�S�1�B�?�|�Q�'~���s��r�� �C<$�P�Y�l&���s��p抂�S��\�����VL$�W�!K��<X�$���;:b30�CF��YMt������+"�] O]'��;ī�z�ؕ��WF<c7~#}ks�C���$8�6x�>�yU�sA������ip�5�@�K��n7�����>a�jE���~��{��PFq�j���R��l����Ҏ��[^i-��p�椰^�H����҉yT��G&A�w�Ӥ���*�A5^8���˺�/�����s��n��+�!�U��n�⬥>�+�]*r���=�&�����rru�`)���s�3�V�`t¶)^.n�R"����[�0�ۍO�Ȋ`_��d�n�Ö�JƑ��m*y���
. ϸvZ�hĢ�c�ҵ��#�v�n|#�J�����Ii�c� �
��0	���II�U�LR�}��%}�&�=�jy��2~g��e�ǩ�.|k���Dɉ뇑9)E����2�	��tLo��&<��H��t�Kt8��.��Fװ����������<��W@�ƌQ�+�N�6U�$��n~q�j� mn`r�����}�\�.�����\RxԢ3�1F�����<7YҚ+������A�}*#�������!c&
QVB�O�n'ai�� 	ܱ���p�z����g�pnA�������Y��m���Qă�,?��@a��MU�⟶�ռ�0ÊcMݥ�^�:�.��U��~r
b�������)�;��l���hr���e�+�ң�F"�֑��d��5H��ko�;�����c���ﻄZ��C�E���r	�VFdƧ��f�-�,����S�[��^��y���������U�F��F����茈�h�<�Rn�B�V렍z;�8�j���ז����D|s�����4���8I�V�oTsG&_����D 3�Kx�`�g5�p����7[�Y\����j�2���	�o�]\6�r�˶����z�v��Zp~FH���4^zvp&�b���&�^�7����Í��2��ff.�����
��I7Sȼ-+m�b���j_�VAܾ$l�c<��ǚ~&�U OُB)W�~^\�Y`"�ċ*�;`���j�p��~� �D�
endstream
endobj
115 0 obj <<
/Length1 1719
/Length2 8919
/Length3 0
/Length 9998      
/Filter /FlateDecode
>>
stream
xڍuuX�k6]�)�t��twww��+��4H�tJ�� ҈��tKwI#%)��������w�u���{f����߲0������m�JpWO0/H ���/g@ ~^���� ���׃�bEx��bp�P��M�yOՄ�Լ�`~ XH,,�@ ���1��f����]�8,�p7?������ ���0��p�����4!��P����g�>���������nb@���/�Ń�p�����<zP(�j�%�q��#��`���˥��� ��{�3���q��jE ���U5 �nP׿��_ ��'��ѿ�\Clm�.nW?����h+i�z�zr �v��g�}<�s���~P��@�5����s�����9�R	�����]���..PWO�_�S�!���7����N�p׀�=�����;/7��+�����7�ބ���	�@" ! � ��u�*b�����2��p����@a���� �7�������o��`�� ������f��_�~
0_�9�~� Я�?O��sfwu������@cE#�D�㖓��x�� >A �� ����;����V�����Q���K����G��������p ����~�� ���$����/����������{&%/g����P�/����7�~��<�D~�+��K5����rpg����zB�wE�������y(�|�v:0O[ǿF�?��O�s���=`�^> 0�?����u��x���z�\�]R��n�k�� �s��{$ ߯��������p�����@�=���B ��/�o$� ���D�@ȿH ���:+�� ���D�@~ ��(  :�@���P t��=��x_������/������k����������������������^��K������?��
�����N�m��ׅ7]��R��lI�d\���[ayv)�Y�$���̩�Q��+Y=nђs��[�������O���^iˁ�&q��y"%���q���q��X)�I�6��[�ΡK���	[]�L�+�k%a��&��֚�9�4�F�W�<?��l�<d�_uI[���~��Ũ.�|���SM���߻@�I����q��I�(�K� �Ym�P��_꽛E`�L"�k�U>��)G��W[0[�m��1VO@�R�̵F!<�M�#?.}��N��X�r��+��[i�zq�A9𶁭?���:�{�P<��=*P�d�q;m��f2�w�y MWD��V�]��aq昐��0�K+�\K8]��,�tR�w��;;��XK���Ѣ�ԣUA�ڪ�TK�ye'g>�����J�Pꧪ_㵂e�A��s�fzS�{ѵ>�9Y���#A''2/�Wۜ�bֺC�� ��F>ԇ�ђ��Cm�D�M!ۊ�hg����|�~�J�����4�}%}�9:ބ��!�^�7?��Q�"MZ��;@�=��\?���f�<W���B#��w�ǻ���wF~�j�3K=��{}�̣u��ቝW���w��0q�G�hV�m��>.�;�A�6�H*�Vf�ۄ�6���H�.�D_T��b.t�HџW�*Ҹ��WJ}�R.ݺkV�,��m�0�%I|[�/ms�a�$+ܤFZmbmze�tU�r]ik�bF������K�C�I�`<�|���2��&�R	���#�KV���e���*Ѓ����v�T�R�,,�7��E���ӏyG/0/2+CUX�2���MW�ת�,������M��8+ލ�p.]Chz��Ǿ�N�Z���X�������Mf<؝���h�=�y)]�X~�ǈn�Hmjb�"1@������C�x����lIO�~Ø��l�{Wl.��7�UZ���q^k���éӰgN�"�����T�����7�8��Hᔴ�C��`�oK֓�_Ui*����Z���ڵr�!a�!f(��ܺ�[��F���-&�)���%m?&8n�<7v�LIȄ;�h�X�'޹�/bΥ��C=k'�O?"f�3@�DCe�1	¬�v�(�69&p!	ȬX��P�� ���67�{�1�q'N����s��B�F��k��;�������RI[#��-�);9^��:������ݪ�`Bx�5r������xs���짵��"���$�� �p�;�T�R1�)�8�hFS7�|S��@��0l�Е��+pd%�Q�q>J�ΰ�\�׺!^�,�I�}��ψ�ӑ&�Z?$f����+m�r_� 2/WyL�?:�RM��I���^��St�V�@����<Lt0k������Eu��d���9���km��{0�D�8,B`?�yh�s�T�Ee���i���h�\�7]H��~[�a�B.d�����!o�h�e�u��J��juPC��I>K���m�q?�|d�([5b�2�~�3_���έH�sM82�#�۽"��Z�Wtl+���?%�KB���ԯ`���~`!�]�2Y{�8����e۷i�����E��T���%��Эس��n����jd�1-������:��9�S��\� NN2�B��ճ�m�Y��L=v�*oTP;�a{��B�.�ǎx�cV��.�ˤ���\T<�;W}m��I)L���J_�Ѣ��xW��g74oJ�"��Co��J)�G���qo�`���;�̨gL0pjA�'��ȯ�	!�|E����s���٤�E=�c5%�����0���7)�>��ߪ��[��bJZ��v��<��L�T���(�

R!�=Y�;ȡ.x��o��P�w�h&���>�Kgzg 7\�,�ir��N�SC��9�}^<��A�e��l�����;r43Ϡy������^ѐ���s9��{:�Z"2E2m���dW�OIS���ca��Y��=4}z۷ZXzB��`�Q�y<����Z�X_�ͮ�U��~��1Zza[��p�������Ώq�Z9/��,����z:��S����dLa�bx8�h���֪��)�8������������n��Rµƫ�*R�E*�i��ƹ�������h��}Q�%'�W��Z<8جU9/\5�ʥ`��Y:��7��υ'�͗����vu�����Eˁ�G��g}���OZ띁1;���.M�8n�/�K�aǶ��v���mK��,t��?�<C�b�NA��4~,���ݚ�I�Q���~�|ƽ匛v�,��x���F�6I��C,62"�Ӫ*OU����=�}�dp�{\�'��ǅH�T���s��P�[�ۀϝ,/���֎�** 8�Gp��Y"8;���w�fVr��/�oG���'�f���L~D�-G��v�Sjz=/���>p�e�����Xjٛ����Έ$�ё��D-9޾�����u~h����V��gU3�؈��n
�%Wr�������T�h. �E%�S[���/k��8{k���ۗV�QH��1�Ã����Y�+(����D~��@�P�:Ʉ�2�?�|#0�c�'�E�	=x�2���71��/Z{F���5�tjd�G��0ekv���&T_��'_���YGp���v�=Qn&���2�Г�.���$ZW=���%����r[z�:-�3�i�U�.GIŵ渀�myL���6Jɱ[֖��s�4�'�m��i����,TA��.HEF<S�C�U�h��%�)8���A`����hy!�Wi���8z��rU��}w��3�v!T4ky�Y�(aq�H����;H�w,{&�I/�;ލ�	^e�ܘ�gg���)M�&L�}�t�"�Ǜ�x5v���`��p),�)UE��&�h�c��Z�3���!8Ϩb���1m3��Բ��K�V��N����1�M��鱱�|�)rT��g[C�j'�5�[Z��"��V?V"���\ik!�
�׵��#��r�'d[��@,U�}�}Яz�d!�m�u[)� h�V��w�j����tfC�I��$�쓉����Eg��c�~[{<Rq�b��r�+_H��2�$�k�z��L/� �e}+���^�G��bs�u�o�8;>]�>g�`w�vS�y�<%�vN�N��8���{� n98�>�H��Ӓ�Cc���^iQF@����Tl-�|&Ui�~sq١�"����X ��K���Yv�Ÿt�6^ [�ص��k���5��w�<)�᭒�b�����p���G����Rm�a�6�}N�,�F=n���֪��w W]Z^S�'��XO�O|�rP`�)R�Z�w���t����8�����c�=E-e��b*�7G5�o'��h�,O��6*c�B�+���e+��?&[�9(���tx���6�TCMva[&sR�4�Q��џŲ�T>*��7����2�L���~��8ƿ#Τ~�PN4����M�d�`ɼ��Y���4vJ~w����Cb�}��]��Ü+��&�i�S��������j���*0�eE%v���]/iGK���p�o-�O�
�v�O.�/����j�SG�1��m�1~�S��s]p��S"�%����8tߌRY_ꯜ�H��K,����G�Y�|�Q���]o���2c�=	8'v������͓dæ�Fsp=غMv;0���afDn�d���H��������_��l�m��H�AJ���S ����,e�F�M�Q�N{�9q�X+r0���<��&:gۃ��ژ!�,�֥2���̮v��?�:���/l35DEO0�<J�4���mhޑO���7=�f؀�%��{�|��ї|u��]a��P��[+��䝊�-��#>?��;�:��X���&0�م��b�Q�Pϓ!�w}�i|����V0���EaW[�w/�rjd�]�j�w����Һ8�}�~J:�t��Sf=��Fڰ)K��g��k`%T�M#� ���ܗ;�n[�#��۷qU��_o,��r_;>��� �o��*=�H.��'"{����&h�#�	pnv��!�sJJx[{1c�7��-c3�'�^�s�U�롰HRh!�N��b]�W��N$`^��q뫳4sx��	������ɅUG�Փ���2�S"��
*N��ߓ����D�~�����̒L���Xh+VB1��E�<��$��A�\����ՏD]2����~x&���}G��R��L�.D�*�N�Jw5H!��풥���,PO�>F���?��>!��*[�%8����^}�'����^�"��E@��8�4��c���}���7�S�.���a���lD`���iCׁ�sq��S��o�,�.�X�|jQw���cZ/���h�z��S�HŢgg�kZ׬��Ɲ���
<��D|皧�1t#V���ur��z#��� ��k ,ItǼ����IAC�0C!O�e��X��d@��H��H�A�QAn�pi���
��}��N%��7���<�Yw��2kW4f^17�'Jf��9������}Ҥ3_,\
T.�yS���.I�P+^̧/�H�	�h�,�/�n�j:�	�.*)r,�U���T]�6�ݴ�3O��$׷(|3�Ll�Ǎ�K�R`��G����\�f��⵿:!�O�Hyw:��BC��Do�_+4{��˜T�y����8����&�l�fJ%�o������F���2]f q���q�Y�C�n�F���[�
71g
P��i�*mߢ�QE���e��h��ሒ%}���8�㙃���)Y��c�O6_�#���gW*q3�޴�Z��7=A��8����%Fx��*�Ǵ7ey{�=��Z$2:��j��Zb� �mHaДxҽW>N
"ӯ�]I8mk|�
�|�Ƒ�a4胙�x]�9�k���N'g�c�������{����6��<�,Te�-D�4��fKbZ�g�P���K�ѹF�A�:���W�T�-g&ň�0�|u����ͪ�2�&f�dӉᴎ�5߷~Vl��u��,��9�zj��E�]z�Wy�!��K��˕]��6E�Y?��\Dl�����9t�Q�b����%i*V��޽���ϜK":��KBr��q��'�nQɍ���>!>���U`���/�sKv��
e����z��v����x�7�
��(����e�R]�����ۂ��n���8YS�;5p��~{V���̹��se&93m��1
wԙ<�TT��c�u��w=_R�[����3�� �;6����^�N���ӌ��5TR*Z�Læ(��ck�o)����s�=��'](�^�%��%ᇓ�-_)R�Pmz8�J|����UlJ��S��BҪ1�NÓ����r��ד�"w��/1�<�Ss�U^���(����s����y|�m˾�QZ�c�W|�}���ѐ��N��x���=��e�ذfi}�q�zGz}��2�,ۆˊ���p���2o&m�1����S�a�c}��ʑ���]1��h�A���Q�ަugYJ�Cac��Ќts\�دI�>9�|^Em�i�S��2H�����P#�D���h��Jy̧rG3O�����E}~�M�RJ@�,����񊪲^k�ھK=̓D�6=��������t~����]��D�Ύ�KJbgA�K�����5��8R�>ԓ'2;��5��� n��6��;����W/�1�c=q�2O&k�Fʺg��r�&0�"Q~gBG1�#-x�5�1�񤩦�c���񪩬,�Q��/�p���'O]�y�n����g�,� �a�ф��΍=Z�]��<\��j��6*���?Q��5Q�=e���:h=��(nl]�S[|m��T_4Q�D����-�<L���弣[��.r�z���Oi�k?�W���s��A%�%P��"�(&EV|��r�̎Kt�[�6���ҏ
e=nmҽ�c��z-'��0l��{��0���٬H�'0U�w5�oe��YbzS>�.�4 Ul>��iV��t���k�J/�Yɱ�<��"'��'�ԀC���+ZiK�d��V�m��>F�����#��v��Q���d��i@+��[O�B"o�\���?/�VKv�a���$h�{n��ƥ�X�]@�gݏ�.)�v	��=|�>(~���t�Vx�޼�����ڵf����[h��7"��pǢ�ݞ^�� �6���� _X�����=m��nw�Ga�>k��n1kA<��Q�eO�{3KM�5b߱d֪�U�Q9�b�Ð�_�t��̶ ɿ��,\�{�:�:S�J1���~��|5��A/�@��;�����о���ANs0>ƍH�b���w�ҳ�L3����)�_Ą�7�F9RVN�Ã,ߣ���Z�tԽ�{Vt:���J�w��m:%���	ﯩ�s�3J�m��ZV'�j�j�Y���;��d����P�%�Z���ߍc+���S�
m�9.f�|�^2�Y}^��	��m��vX���Ueá�qr���T�
Jx����c�N�?����,��$O[�X�N�k#��THڍh{s�.
�]�$_�?���E���z��c���f?S��=�0�\�P${���0�b'��>�E�C<A	m�PH�[cG=����x��V߸�i�J�t��{§�M��W��?rK7�q��Rp;b4�{i�G��_�_��}R�N�D�c��Ȳ�����@~��O��D9}4���뒌u:o�����cЯu$��J>�~��Jծ���(��79-~�=xk\q�)�`��
�yd=�;y��+J ]qz�Q �ږ4�)��N�E��zH�W����Uu7d!;q����Y���b�I����=I�n�����[Vm���q��wؾ�T�C�H,�'n�D��N:�b]�K�.Ve����Y�g�9s�<}�i�il��?��p[5!]���� ��Qf�vI�TM�=5R�~8^�J��gM��2"�xm�H�����<!L#y\��
����0£�]޳�"x��=�.��a�ǖ�	a3`��,���8IT��)��O�*��%��)�bA0��e�{��� �7V����\r��S9�5��Ϙ�6�"�LgI*��Z�>x8G��%:g1��<�"`�y甼a/����(%�㻀���iί�N�O֎{P:i�d30"9�Pc;�6��9������'�%LA)훠�Y����i��0a|6�h��?Q��](�bl���U�H���"{$�a����Z̠OV��FҲȴm)���[9t�ȹ|܁�x�)}��l�4�Ĝ=y�@�3��#*��K�]!x�����qcת��_�����R�]��C�� k���1R{>W�7�F�����볇&'�H=���I��y$N�S�,�~�S�R<���%]V�=�'J8	���H�GTV�+�ЂH��YM�������M'�� �܉����V-Js���bwu��[�O�c�Ml4L �7N�>سueÕ�jF��08QD�>4?+���/,��'U��(��0����vOR=�j��J��ud�x�y��a8�-Ӵd�ŵ�>5�")OqH����"��O-w$�+4����>��h��_��[T��L�kL����;d����f���̲�d>�F�)�M~| O�ꪓ�ZM9p������)w�R{"��cv��sZo\����q����0�%�� ����{\񔜅W}ٻ[/�?sg<�\���w'�u��k_uq$��B��Efa�9������Q�m�Z�p���G��$Jv�*�
w~ZG����+�#8801;�c/^#�Q�	����`���['��V�R�Ӫ)+ꆴ���E��x$�c��V�"f�����Wp�v�`X9!����S��g�YՐ1�t����є,E-��}��x�W^.Sg��7{Y�n[�^��H��#���Z�Xd�s�Ύ��f� ^{���E��*�wm0�����u��}�9�ڈK�5n>��ym&ַ��7J��:ORw���3���7qn�L貗�������r��RN�ɭ�|o�N�mv�h���YBX̑܌@&�~��":*�����w��܊'h/�5������h�td
�z�i���Ha�&�X.;���_K;�����k���/$s���p��n��>��V�Ԅ{��I��m�;�t���15�e��1~���V��|��lީ� �j始NJ��xx�֐U��!���g�-_��YC\`���j��:�k^5�fU:�t��ӊru��K�-�)�N�u����J~��	zn���Mm�'A�A��|�[����*��ѭ�G��j�x��`�l�B���Q���Ld����Sa
@��9m	Sp��wk���p ��r�wn�Q��!8����r���F���pY�o�����q�lba|��'^"�V�i���~��ʮۦ�vӝ\"��4��u��p��?Ih���<a(�rĳ�I�@�SE�6���$}R��t�?�*��L�b�_L�:Uq�s����rX���!>���VRo��QRch�[.�������:K��X���|�+!����)���i\�Z<T�.S��V�1b�bj�xK[j{���P��H#�T`��zT���@.��y�<���ζ{�ׅ._:��(,ugZ�Տ�0�����G�������4�<�o��n�>�fL�CX�m���ܮ~i�ߕZ˷P�$xr�I�A~*ҍ�N�3/\��Q��q�������d�-h��q�^�/&�K%�s��y\�㬫՚���r�zm�5��w��z-�S�"�\cq�0�2s����6`?P���G��) J��\�/�i�n��MhD,S3K52+=�}��Q�yD�β��M-��]�q����,`���u�M�?�E�T�f����F'�s'�Ҽ��a���J�8IC6��O���ǌ��+J�}ǹh2H�tz�=�#}��^t}�'z[&�/��@��u�r(��׹�y�Ys��)%3���Z�C���oqc�l4^&z����$
endstream
endobj
117 0 obj <<
/Length1 1393
/Length2 5926
/Length3 0
/Length 6879      
/Filter /FlateDecode
>>
stream
xڍvTSk�.H �D6R�!�"J�A�%� �$��{S�T�" Ho��*E�HW�.E@���G=�;���]�ޕ�v�;�������ޛ�K~]�k�R�bpׅ�0@In,�`�PL��k��9��2�x����X���(��8�M��㴰@��	�%d�%e`0@���u���h@
h`1(w����mg�×���G
 ��Ғ��ဂ3��D` -�匯�D8p,����+��=�"#$���E8�C�nv� �g��Qn�(�a@���
�����p�-���'4�q�Gx`lPn �8 W�踠0�w�  �_gC�����_�И��$�����1v�-�	�ށ�q ���D8�c��O�	a���9PU�x��sG��]p�Pw��/�B���OYc��uvFap�_�SF����c���YG���������E���E�.��RW��7���١p�8LZBBL@�(o��Я�>.��N�_f<� ?�`�'�
@ۢ� ?w�'
��y������+��0`�F� k��';ތ����7������`�~��3����q�����B*F����0��OQ���ץ�� a11)@RZ�w������o�.���`�$T��b�?�g�7ϿT���� �����K���|3�8�����w��M��������R�pr������?�g���_ ��=p�����g��PCԟI�B٠=��۫�C�Cc�W�ua1(L�����F��qH�?:���NhJ�����G�`�����Pq�7�����U� �6��OD\@��!|@0��D��?a��ڠ���b�8|�� �b�@�ڊ׉��Oo�+5���?w���������P�($��$y#ܡ&���J����� ��bkT�qw�8�o*���Y�ڨ���M���i���L��~=�6��
�K���#P�^Vܾ�����;�e,�ݰdZ�2ݷD=0��)���d�ɳ�Nѵ��g?Z��L�7g����+��P!�u�B�1�"��،6���4��@����1�C��X�c�eӸ���پ���4��<ͮJ������\�o�zY��4^��)�(�$\a-p��7Gd%9w:��Ӕ�+�Qg��B�8���	�~ɗ��fl�*-H���Ox#iIѼ��|�v[��UB)���C+!�}�PR@�[I�"Rb|��WN�-Zꛗ!\e1wiDzT3��S���R�����W[z<�J����6�����Ҽiγ�A�����Y�^)�Xy�8tzԻ�%m%�D�;&FԻF�f��3^uV�r�Y�j<�T��H���\��&�z&�o��_@(�f�;�|t6�x7�`2��n	�򬙐�<�eJ�<��jE��TNe�d�����.2����6Y0=G����h��NB��V�a����oL��W�Z��H�G�6��h(�1ICnQ�//�_s����2�ԫ1���U74�Nݚ.wJ������I���E$)M����ü�����<��D�&�����ɿ����r��D�jޤ �$�r��	�N� 5�)���&��C�#��)�Iގ�č�s�0kk7���ܝV�NP[�*/:^S�'-��m�v��~���$8�_r�����k�A}pf�h���3,��=,='l���L����D�����Z{�z�h�\�N���ˏY�������Zm��|"�~*����<���q��v�aR͖��?e�d��/��y�`�4D>�yݦ몭���H��p��Lde]�����5��d&R.����cNTP��[����9=��!�.��ƥ:Y�O�u2�S8��j�`��j�Jw�T��������l��\�w��_J����ő^k�m��H���V�>)�"+J�r+d�p�����6���]�i#㎯8���ʖFQ�YJ
?����7?-�0���� �z�}���>��YO�2��� [�Jyu�U1��tO����K}�Jf۷Ev�mО ��k-_�� e������|$���*w�h���/��T9K_���4�O%X�z��A�\�����J\�7���\!��0l��!�~ha�^������>Ws�h��Tw�`�����nŎ��F��$
�ݞY�u�����;&7�]]�#�W���w�(z�u������m�W���w��&U�hA�a���_�>��
�c��D�h��e�!D2�W�u�FI��tzL��V|�6t��Bʚ�̛�#��?��s��΀{�J�6����չ;c����%�Wԍ�m鋋�ϭ>�h�[ŉ�^�׈����F�q �Z?�C�T�ܮT	�ѫ�F��,�m�z#�J;3�܁.q�舭mS۩�m�[C���l����"���(��vС��H ͮ���%!P �P�S> m~��?L��%{w�݋z��N�z�@����f�H
E'�L3N��#/�ܜ�5���2��Ü�c孤��TP���9�]Jz�7f�������.�y��R!X�{�@�o|Z��1{�2�4 q��⽠���Ku�
��A�'u�"�Vw��D>$����
�Rm±ʊ2����AF�¾��nL�m�IFХ#�����jр~�U�g���x�W�R�hX�~ڒ4���k�5{|��"�!"ZT�)P��c�m0�^����Vr�ׄtd�Ls�7$��EM�jT�̔�L��d���b�CV\U�8����V>I;^���}~�X�����⽲q�8��|}4$I��,I��$�q�������Ќ�3OY�KN<��`X����[/����z�R��"z����M�2��˔Ί������_ouU(�r1Lr^SO>�"=J#x4;�`��M�����T��B�TOp�a������f�ܘ��t��˪���HX&����N����\*��VL��\�ǋSЉ3�M��T��n�p{:��O�<�������P�}�7.ls_��p����u�:�V�2�ȃS��Ϲ䤞���sb��{�{����L���複��N��%@���� 1�5���%[�?���s�	����߻g��~����[
���zwξ1�d��"�v�!3=� �&����z���+���;�M��W4�#�U<j\Yx�N/��a��S����W���!�rԇ�����c�
H���w�g�s�.��n�"�#��#��j�������]m�V}��%[����-cF��S&�rJ��H{�.2w��ID�8�t��&���X��T�N24${Zz�5�]�!�I�vF������7d��F�_Ϲ��:o
�}>f��l� `��r+��եyU���D�
}��_�
�V�ëQV�ת5�j�;�Զ�E���vD����G�^�S�����2�i\�w��#���t�t~��όi��t�o"�o7l�����ߍǐsR���e�q�Q}[u3Ʈ�̇�/�^�X^:f�~l.���qv`T�h�R< &����R0��2�Т@�Q��.�ڸrM��^�ǌW������ ����ܻ���,�)��I��3BI��N1}򝾚EZ�;��ሠk!H=�1ZPbI.HS�#�]�O����N�� Í��zc��/k�2M�]�&GNC�����OY�&��̶ r�0."�NE�-�u�����Ȩ����퉞jKMX�G�\>Y�������Ȇ��d �!��� ��z���Åi�&�uL�(/�mz���k�a���Ĩs�]���=z���yHd[�%�Årw7_��iH�<��"�B�Fv��v���OgK�br=W,0^�dk�w\k[c�����q�s��V��)�7��C�1B��w���R;\���7�#��t�?i���lVr��G��V�58�~O[>�c��	�D�t2�ji_��k��վ�|�[��_o��Zݿ��xS�JqRXmEy�����n��_�g4�؋�s�c���3 ��JDm-�k�@tF&�)-��p3�!��VM��1�j9絙ި�5�4�(��8�<6t�[p(4��5!b�8�� �"z~���Ra9�?DH�%*�f��E�I�/���]zO�<{LV�$e-��X\:������شC�Jps�rEeP���Vi�쁀�r�J�K!�z�kh���t���晈�VL�go-�0r�m_��Q�����t���`鑕y$'��~���:w�o�U\R=Ӻ\���xl؊�N�RBͫ2<1����lb��[jmٽ�߼��6��}�	5�T,Ip�<��i�h��":1�+hB��7J����'�s���w3߭~��z5DH��j�Q�L q	G�e����:i������z'R�j�r����ݗz�<���&��.HXB�����&Dȉ3]���>�I��:��b��{�������
�ݰ���C�2K�>��y�BF�8M�B�kP�R�R1h/a1���`�@_�޴�w話�U���4��������󈕟��9$��=�F#�d���:mDgW�N/k�����g��+=SC`Ӗ6���wn����o=<(=N�b$�IC���g�g��+
���:NƧ�n+پ>-Vw���fü�#��U�FuJ��k�Ũ�K'<0,Ƨǻ���4�JǦ"���0��s�R�o^�K�]o:��܏2�up�1˟��5�:���Uf.菁�w"��ƫ���t�����C���D��\&�t!3���m�z--���L~Yi���
������,�HӢm��W��FRp��O�gĭ�e�=���풣�Bu`���-�:Y���vb���%�	vg�\|������+?�vuZ^��t���:ѵd�<Ӯ��h�OP)������Nɩ�2�ʸ�͹[��3�1�;������>#�ğ3L*NľӤ���f�q�o�[�$t��E�l�f���V�^����U�]�1K�'�Wa��.������R�2�պ|�l�4�r+��0�t��h.��X\I =a�6z߁�͗�KOޣ�<r���Q`ڪ��y{�9W�O���5"���:��:4.�X��Ħ�f�zЂ�Ir��Ӌ��%����!����u��g��}Lu�*R<�ζ̌��m�o�����ǃ�޼`]�Uy��)��fx�%�Ӥ�0�2����U�)�5O=c2e�>x�l}���x�0�<����:��Q�)Ҽ�T�G-�v!��v@�%����s���i&��oF:�&���ޯ#9R��S���sW0����l�rɔK}O3��/V�{���P��IK���-K�.��\�6ݘ��J䌒����G�O�{��<�TQ� �xv��w�,9D���u����4�R��)��|��8�i7+.��T7P���<��j���Ij��������Ts������M|���Fk��籝:7�6��X:�Ό[��5��z��^���Hҭ��Lm���_:S���ζ8P~�Q�+-'��.q�o�r��Gx�)r���������J4*�iί�h�Յ�CY+ʎɧ�������M�j)kQR=�$nȦ��,���;x�|��-��i�͸�=C"�t9@�`JZ�
#�C�W�;�M��e��1}�K�ck�5�nN���5��mm�5�8�.�f|̥]vD�����g٘�'w��'2ɛ�&}�W�ϟj>������-���N�L"�Lwxep��/jfإ)����=�ë��6R;���hΪJ�l�Uf0�gQJ�9��r�|��8���4���Q�o��������H��F��ȧ�<PP�r�Bش2�Ɯ:�:�A�O�ּa�ԉ���K�Ė5���;��܌���w�IP[��$5�M�Kur%s3���{f.����l��C	�بnE�,K���?{�n6lT"�]�|�板�Bd�N"���|Vqn���ң�ֶƟBٌ|/-)���q����;`�q�霪Or[Vpүq�h��J���ga*w��H�[3Q������?[1���wUr4t�
�|K�i�F�+���J�s}���J/ș�(��jG��N�L�E�VV�v��}�Af���lV��7��D��;Io:$�y�jz�Owc�D)���%o��%��6�N�[<]U�S�����uE�u:*#+g��^�E���O�#�3ٴa ]����g��K�ӏ�94�\��C-K4]oP(��KD���p�1Q'�L�#6
s�֡rg�,�_T��$~����F���ŧ8���:�<Y��Y�V�^����-�}���!@`�Fn�uj�XB�#��e�H*�#IG�;����<F"���xhH[�}�;a��@d���_�E7bau��J��UR��GĽ+�D��3y��t��V�����}��#�*��T���PV��f��۔����Ԙͽ�^����)�ܻ
���TO�|����9����ֵxE��MεgIt�+M	-��6��������D8��5q:��e�rE�Z���M�����j���z�[���0�mc����l}�`�r@�3����E�jH7m{U�S\��z��c)��I�8�ʋO��;U>Pz��q�'c������Y�F<U2\+�O�^9n��"\�e���L�+_W����6u�e9i���\'*�|��.��x�����y�F	���,��+v��[5��d��S��`��Jk�������P�]��B-z�R��-��6���ջ���mɛ`�lF*�]M�H�b%�� �����Sdn����	}j��V�E*�]9���+����y�K"K��˜֠Ź
�J��ac-��c����4Yڂo���-/�ftQz�7����߹kO���;��$h��|��M_�N��n�*j"m�p������P�A��C��Z����B�
endstream
endobj
119 0 obj <<
/Length1 1581
/Length2 7631
/Length3 0
/Length 8692      
/Filter /FlateDecode
>>
stream
xڍ�T�k6��tJ�� �4C7JwH70� 33C��t#�tK	��H+(�t��H|����������Yk湯]�����5�L:z<�vp�����J �5�LD @� /(��ʪA�����Y�$��/y�Bc
 �O��� ��"��@ @ ���� (�< v M^�F���]�G��ߏ v[ ���(��p�,��؂` M�EW�� ��0��_)إQ(W	>>OOO^�G8�pp<!(G�.	Fx�� ��@P�f��� }G���Gy�` p�؂aHt�;�� ���T5 ڮ`�g�?܀�z����O���%��~�lm�PW�s �C\� m%^�� ���r� ��x���A;��9�$� B���qE!y��_�~�AwYf'�B�0��� �-���|&��{�|�:�C`v��Hع��� n�`U��\��?������n ���#߯��ޮ��F�_0����+�`�&��؃�?��H��B���}����>??�b�؀ 0���a���3z�����? ���'����0��ϗ�H���P��������^ _! ���Z�� Q1����������: �_w��Pf�������4<�R�_��w-8Z�` �?�7
m�_�������'�_Y�o���)����6����� (���/���Q�Є�w���F�?��	���C�ת���C�V8�/P�A*A��v:����=
t�GB~�p�Q@����Kg�~� ��m�w��ua�p�_�' , ! o| Zc�� _~��ځ�~������! 4G�=��k��� >t��ϰж�0?P �g� ق]������]�gAغ�]]ܑ�x���@����y�s�Z�0q4uEy#���h?(�_�в�sE@Ѓ�G�w��j�/�_m�uG �%~�ݳ�Ͽ_P`��nn+�T�r^-K�ɳ6����o�.�b���먁�Ry�M�ڮ��p����ЉY?Z���1/���g�)*9(�ޯr<y�?�:�v���ES#��}{������D����.����Wm�S� P|JʔB����n���2az��21'���0Ks�W5x��=�k������,6}haa ��7U�0_��J�2����G�){8����E�Z�MN�T�49���ʶ���槣{�C�J蜆e!�*���N��[�^�@g_�$om�g�,QDtϧ���^y�Z�k�c���;��MP�@���X���T���`dL����6����QsW�d0$�s�������>�mZa5��x��V+��7��ͻaۯ�ex����n�L{^���h<x[�)��<�E�<wz����AǛ�$J�jx��o��Ŏ�eȿ0����NɑB��ʩ�uȌ(N�#Sa��dsu�4-���b�ȫ�Mc	����W�!���+Rro�mR)R�i��Kkk�dl�ߕ��|��h�f(�KW��r�B/6�2ٹ�8U���NxRʅ)�7N��,�
}nF������'�4�Pb��pU)M�~&��1���]q�jnd����r�݃E6����Y�{� ������{'b�Ӭ�o���V	w`f��dr��X����(��+h���\�Б�������i��W/tss�M��m�}�^WrF���1T(���+3[g� O2Z���w�\��r<{x_v	L���@S�Vz� 9&�m�|���,)B��m�����e�2��Q�;�X�� ŵ���DKu��C~|����|�rk�}��B.-�T�ފ�NH�I��]�d�UNT!]%ʾ����#7�r$g�E=ԭ{��K�t@h�F��i��v3ή�觥m�R��6��H��r
oe�,�C��]��y"�ӈʽvO���ۆۄJ���T�I���B���Z�#'�
���;��"�LL�j��B>�����N!+'���z���:#R���-O<�T�zn;�I835�\N�S%t�wA��,����,��m��n�L���͓�8Ì���ON�4��_���%��� �r =>�\`"x�gҶQ-����Uɳ�-�#��-�)�4$��NQ�����1�;�
�N�X��zYE����W�l�Ǥ�����.BRMP� $e��������"��-ݻ��ߚ���#��?v?� ��2x�4BY�|�Q�=g.@�g�`E�mؾqnA�d����>��g�S�0��z�oA�~.���q.f�������ϑ"�U�4l�yH���R�9�8�T���Aa8�6�JkI�i/������g u`��R�g��A�����{&n�k w�n�l u�X��:�<A�3�RO�|��񹅫��
d����z��)>k�$��Q���t~�j&�d5�#��g�^&��7��?����2���Vw�$���a�1��a��cy���O��E&��_k�C�/T�m��p,�'�"��w���f�O�&��f��\�Ae�1Y�c���]#Y��j�s>�	��c�9(�{�ڴ;?���5	�+q)�叚V��9��^<��H��&T�	V2L�����p#H��L����Z����4K��"r���IQ��e.%w	z�?�����ߒm���9-���!oΜ6j���N��������x��X�99w��"���b�z���e"�
;�ٖ*%�םx�C-Gk��ˏ�m��o)'X��;<�ˑ}���g6�Q�����^�[̗�c�� ����66T4�=�O�N�{eh��N���o�oW�IMz����,�<�MϺꉖl�޻��W������Q~`�[ӑ>�9ũv���J,밈�A��	RB��DE�����h;CP��js����;%��y�3�$Z��Z�{K���8�=ܓ0YJV��E�Nr�x���stGR�"8W����lCk#�tr�}�F��2�]c��e��-9��Uoɻߦ,(?�����i7�Շ���Ͼ=�R­`�Z^���Kv����W܌�᪖�R�v�P���M�U��D�������9��R�PX�9	[̙'��Ѱ�?\\�P��)=RI&[�;Fj��svG�-�y��B;�ө�F�t2~����Ӿ.o����fČR�k���Z�%��J��>$�i@y��P���c.����Di�9Glb|�Wꠠp3�wn�In8Ja���*s����V�4?�����_�:$���l��a� F߉�%?w�N�:�$�E!c�a9;Ũ��!��J]�'�Ǩ���z<�j6�����U�4���-�u��v���o��"�liV^����s�M���z�y��9����?��:��Xƥ�=B:4S��,u�E��1EZ��aKA�߹c`�Q��(�|y[|��6������g^�A�0,�5O�A�����9�U�_���rl�",��P'���6�������1���LՔ��m���%\Y�r��(f�^9H�ܷ	u��I��6YS$?I��/��9S��H�nކ��^0.2
9���~�Vj����t��V�Q�f��F&�&��囬W}�����,z�QU�9���:���dT�&A+�����'��q��m��/�.��=?����s%������*�UT�a���?�:p��:y��'i�����w��%�ԛ
L�\�B)��w���.���������*h���N�����1c�
m`��: 	@~�g+�X0���@%�}�JdZ���X[�>����>�*�г��)��q��RP��T=��ؒ�d�1�^�F�n�����h�-%�K���0�a�O]=�.6��L��0Q�G��c�X��5=~�^ǣ'P��ګr�M�}u24���	 x�y�%���@��.1�r����#@d�U;�c*� 3z��-@�Я�nkA��9������ܼv"40c�c{���~������������z��ы��V��Ĳ����:�h0W�Oh�C	 ����x��֙��H���pn��nHX��OȢ~��Ȉ_Ry�/#�/����Ȝ|d���]{B�W&��5W澴K��.��]�0��|0���&g�Qr�3����B98#�T���F���Hs+��ʵ���1�\�^ɇ]AwSx�^�8���^��Q�a�ͅ�����g;����=�m5~��x�|��>�z/����W�M��n�l
%�r3M����xJ���>G��R�?�nZ��tfD+�㼋J��f��#-7z89E��ڧ!�����._�9X�+�*~��3�
ɱ�۷l�F�r*��Z"ui�g�D�""o�ԗ_��U�;�^�	G0}I$l),�������$�2��8�sTu���*x:��ǓC��~ɳ�:�Y��/+���k���RG<��h^�q縪�ڱ�iVmψMjd,tl��}��i{0�r�wɞ��&��.O�)� �d���nq(���Y��:ux�%o3�JU��T��I^��������xX
^���?1�6�Q��,����hY�υ�'�%���N;r�VZ~q�¿���ﯾ�K�֕\^v�1y��s��'rf���u���R�K�ڸ��bh��C΅�-�j8B9��/�,t��፫��{�Hr�u.I��YD�>��)�j��r�H��Ҝ��=*�셎YN�DR������)�H�~�3}���܁�u���@;��ﰲ�����o~�_���A�{�s�S.���[��b��)T_In���`�M�X�xνǞЕ���eI����U>���*���zWy�SDҝ��Dz}R�I�4�_�3������pTA IZA7ܹq��2�M�%��C�u[�������{��Nc�J.��u���g��X'ᤳ&� n��u�vc�6�jS9���}TOĊ�����T��^��Ժ�3�����U�&q=�L�{M��|�U�r��ѓ�Y���@�(�`?t"*�$ٟ�� C�b�����8�;��$���
D��{MM+?�����J�*.s"��Czq�J-�K�"��U��`��*_���k��y�����E��^tJ���6���T������-�6��m!�D���B'�����xG�N�>����p����3�݁�}Z�W�K�W�F](]ۤ)�N!��z�v��(��^���t�*}�<0{�7uw�و&��b�'?DŃ�$��6�*�R����2�6f����(/�Y���S-?�02`��5S�1��#|GE[�7c�T_>����aW.��Qo|�+�߰�_�:���jvk�uc�K{��ޝ�k8&Y����g5���7P*�S���<�NM�'���^&Yl�/��K}\��1�N��	�� �Bƅ�զ�8�z2EV[x��O�?s�,ԧ����N7U2��4碃�����}�1�����4�9��"2����8��Z�j�E�����؎��?�0<�#�_K��r��t�ҵ�X݊�)�����`$lLTt��|��Oar
Kg��5���pO���gx��aD�KF&1�3������IW3��rf�W�<�(c� \���G����#��@��I٘�d���\�|	�a����-��W���x/�'�J�������8¯��Qd܍M���MG�4p�% ��~2/|���n���*ՠ�6^3:\$P��ڬ9ٕy�,��Bc�~�15o>������_ܥ	<���?h�ߐgL��̒�%q@�P��>��:��vMi��󣤞�1�ُdc>��(ޯ��.���sfx˃����e>m'u�2��k�raOk�)<�^zzy��xE�]�R����ی���c��������u>ƾ'!Q:v��Q�>����D��Z��p�,��80v^��w�EV85��
k�9�I�7w�������F�C����X���z <|S�����[�U$H,�Ȧ�|n�l�'l��"c�4�{�9���EP��_\-O.:�D�o4�!p,SvE�%��,��O�d�FUK�P���i��Ď���N��(�ӻU�_��xp$Bܯ��,9V��.���ԑF���I��#��D惒}�7Χkm`�Z��vM��o�K>rn"xe����Qja��dr�/�h�����?EH�͢�<KGC�j�<�q�� s�+d��^h��>�M�pw�]H�����Q�ם>�k*����;��G,����$�*�˦9�O=a7��	[���BY�~���)���f#�L;3Ɓ4>YE�_'�;z��ܨ� �O_�+���p�aק^.wRl��cTO��o�m|�z��׹k�W?��e#c���g��#7��W���ki�j]+�ڕ��j^2>�l$\q������Z��l���	�f��is���ԝ���l;�Ϊ��E���F���u��+�����Q�L,���݈V�([S�����	(.����5���H� _�L|:�AE�2�`!0�̺/1Vfv1�#.��Β��k�nt���e�|y��L�[Ǫի��g�L���¶u]>�O�9B��7�c����\`�F4��ւ�Q��̤}X���A��)�0�JM��D.K�2
Zlyg���q2/�0D����׵�C�dM��������g?:�J���I$5w]�Ü[ɒ+����Cb�q8DY�ׂ*S:T9xpK����1���ۢ�zF����Jˊ�c�|����)���Lq��h�W\����
F[`M�C!&���DՉDo!i������L�uT��y�0��יE�.@?�[�T��l���K֦4v5xh�KxF8o��+�\���;W���2���#��L��+I��F|�E�IWN��}:!�`�L�/�r~sdQ����_2���R�:�� ��ĳeS��A�mun��ɻ�&��!�NT{�鍪N9�+�Uy����`AN�xm���OTw�u/(f	������ߘ�5ć��׹�ц��\,�}n���.�2�pYO9U,�C�[��6�2��,�P�3������D��~�Ow�9�y����|A:�qN���1J;�Vv�;���z(����s����:e��P"�-�v�w4ok}T���3S�ք�&��Z���l���"�n,��v�Et���-p�5#��(�zo/���ҕ4{޴9�=}.]�$��h�)yٱ=����WL��u�ބ�r~�"w��U�����4��Fu���u:�$�I�+n�Ֆp��;���yا�m2>^�ݼe��tiu�� ��p����;�Ńl_iM��B��I��Ig�\ĄTi[�q�N��Z��+?k���_������!�.��(����u}ۺ���8�P~��}T�Md*^* ���'�3Y���pOS��a�6ͽ2��x&�U��dEζ��S���rL.�S���v�����q����g$��j��#QLb�u�<�L��T�<�_�`�b�c�4iy;D�#\�42Ͼ��>�if�F��}��ݜ�OmYď�G5e�Z,t�6�uQ.��D;�IT�v.�Э��b *�p���	�3�]QPr����Q@:�YL
�S�}���If�k�P�/Ju\<�� �4�$�y	ug�^5c�\���Kw��1��G�@@G�^@�����.N�LҺ �Y;� ����Sy�6���iܥ�E7�F}F����
�2,��:3��pݪk>��9�t�9�'�fc�c{H�[�]A�}��6�vR�{�պ��Q�|�@^���0�Mv��.�qM���*C;�p�p@�����Otz�nď[c�2�m�6��?%-bleX�j��ŉ�wm�8�$%���ӳL�>&{���SšP����c�e���Ěe�׮(�6�!���͕���[���b�R��_F(�j�V��Ev� �_r" �~X#�m�N�qok��*�mr?�wtGC��JJ��yp&�1���K��g�E��$5��K�Q�O��zE���<Qx�D/���eg�Ҡxe�"�������_���:iJC��.U��k/qD�ꄶ�V�ܠ�ӝX�,'�	�cҧ2�VK�ɹO�j�[�)�+�Ds�Yu2��g�{�b�ē��s���1�x0qE�3WL������^ћd���ϡ{�����(A���U95��������������4��Ϗ�����IO3P@c���\z�O��	H_����}�xC�X�i�;��l,���:]Z=����b	�p9�۪����^��xS�n<��k�)��p�7<<��48>���:�1T��͐��K��H���p�J��w	��X{L#5�(��'�/���U.���y��z�aToQ�d�a�����~�/��k$謥��z�M��?._^&�Kh��/���e=�7u��Ƶ,���o!d��P�!\p��8��[�',\v��g��(8ɇ�*���qM��0"`�7R��Ͳ� ^B�u�Cץ�7M^�0e��R�:5#r2i�(D�VA�{����|Yk��M��D�S�;?�����zt����KդBM��[BL��7��9�.����ٹ.x�,�2~���bRw��wN�&��O��?-�<?�ԋ\m}e]�_���w��s���O���|rF�o���x�����L�ij�˵�ܦs'�~�8�1_�OD�nd}> ێ��LC�28��f���6|�yF�C�>���+���o�_ը;�Y"�$f=�M�$`)_��ej��L��(���3�T��a
endstream
endobj
121 0 obj <<
/Length1 1577
/Length2 7339
/Length3 0
/Length 8392      
/Filter /FlateDecode
>>
stream
xڍ�T�[6L�4
C#�ݍ�t��� C����ݝJ#J	�(�Hw��H�"���z�y����Z߷���y�u���}_�vf=C^E0��C�
�	H�u�% �|BD��FP��/����@B�0��rPF@�(4�D��t�0���@P (&%(.%  ������ =�`�@� �ؕ�n>��#
��ߟ .�}����8��p��+a  ���t�AP��_)�dQ(7)~~///>�+��p������$�	~<�B�0�#b9B�pC�=��� Ѐ�!�00@o0���A`���8� ��@�O�tE�J���@pW7 �
s �C]  ݇�|(o �r� ��x�'��C;�>9�PQ D�������|H��/���Ҡ��
+�]]!0����T�]v�?�u���`~-�0��/`7~c�����"�� AD$��D$w ����+����Q��f��wأI@����	� PH����"�� �� ��';���Y����z,���z���F�������w��5��=��������+, ��
J���%�N�O�&��B�:��2j��� �?������_���kd����#8Z� ��o% * B��?���?�������衇��o3�o���t����倖�
=:p�p�����g�u `����Z5P@�x(������C����r�#��[���
�����_7:J@�l�9�o$�a�M�P�{_U�5}B�b  �!@�L�?A���!޿����Q� �c �� ��Vaa ?:
���Yh��"ѥ�"���q�����q�أ���w^AQ4���]� (�q�pAA����6I�mqs�@���y�{�������/�_y ���p���{��r�@�! �O�p�t������E/޵�/_[c��{�EQ3/����&ݕ�+�e�;����g��y:�mՑ������ڳ��˛?pv�� a����3bV(vR��=�ı�|b:}!�)�Q�^tՊܰ��p�<&gNg@%(�~Ǌ3gf��4�"O�DXfzi�]a�AO�����|V,��s{{2������*#��n�j2����Q��j6 18�D��_�%���Fr,b{�>YJ6�o��t��	;)�2�(0X��Nj��v{^/�:�P
6oG�R�v4������Z��Dt���D�M���)����K�7z7u8�(:��xY�ʤ�*?'����U�:���=�#�����70�EVPp04}�dT|js���#��>��{t�]_N�n�-���z�������aY��:�>τ���� 
c5���I�}��4D/v\Rёڻ���bm���J6��s�)�8�a�<3�s�V�l/��,u��D�3h皱�hmF��x����w��
������i����!i�9}�XO6b�	�V0�[���2~��U��ЩHY�ť���囀�:�丨W`�^���8�.���s�7�"(7�1Y�GNF�'?V�����{��C�_l�ޜl1��\+��7h<� gV�!�5E���l�:	K�;�+K�]Xq>l�d�QKR$�$�*+�-�#U��k�x�'\�PWf٧�r�Au�D,��,��>��W�n�3^^�#�6��qi���t-mYa*�9�D9-���L�u�0
��&��|��=�Ԣak�&�7^G�IL�͖�3�l�ߒhg�Qr�"��Oi)���"Ŏ�Bw��Z�����F/?��\���A˙xB��5�u�����ۛo!����|g.���"�r-9��̱$���mO�fP���˳���9���fr�#��$�1J�e��d��KF|T�6a
R�b�(�爸�!JwQ
~��O�� ;}��:���_@[-�����C>��8�舞�Ԑ�:V���|O�^�Xҥ�-�m4����bR�h�Q��xm���$s��t��8�2�w�t[[�������g~�4ʞ�SV|z�\��V��S�'j<���!�´t1Vp��Y5.�h�a��!nY|�e�����{n�u�,��Cc�4����bH�Ԍ�۝͢u�)tH0_RZW����'�zp� wVG2b�*j1�3�+�M������h[�6���랹�|�OS6�@̞�J�Ck��n�����%�:�Ԩ}] ���+�-�R�.��a(�ч����"�qbA�i��7���G;ne�J* J2�٘�yj΄�ő� 
�Գ��:��u5��^��୹��nU��6�$���Ɖ̸%�����ʁ�b��357[�ۺxE�Y2��䛤QT��݋��"z�#���3�K���5b�[��*���@>t���'�e|7��-�!�I��kX�e�Y��j�0��CQ�����=���L��t#ˀ���9T�����쫼x5���ڑ��Sث���q�m�@-.C����J1n�����TO�R��	A�� ������݋�"z�x�;���Q�x
��4�+��Av''FS��2����޸�p�EY����Jm��糐��K��!L�U��\w9�^����fz�T�Z,��L*�ru3��u�2Su|!��dv��|�9%[���|U޾
���'�ː�`�(����k�+z��y����,/T�F�X�yu�:4��̋]j JI\{$�ڵ»���.Mv�q]�2���RX�jv��6~���B��v~L�l�j ���̷t��~��iD�㯱d�Q�����P6l��;?kbh�����H����īW� �M&�
��Ok�X�5X�k{Пܥ�i/EX �$d�m�����n��X�==�~���I�y�O~�IҪ��}������ǫ���Ԉ�����x%�O��+,������VD��<��(�ʐ�4�\-�Yo�[����|M1焉0�77��I/O��5����P����9AO=F!�����n4B4����8���z�7 �j���N���Yv����k��5�*]�sµ�k�uS�4���5J"w�٘-j+�W��C�F�;��8�
�I�z�׃�]=km3�Ji{߾�f��m7s����$�4�gV��j�$9h�Ԫ�����1n�,��8ι�̠/~��#*M�=$�n�{�Z)�>F��a��8#��-b{&��&h
��ɏ����R�8Ԡ�9M��P��$X z��m��mC`Tٹ��a��%��E\���o>�1B��Nh�|�D����)�/r�VY��Ֆ�`�@Ag�.�"�λ�^�fA��g���2F<�������2C{�׺��Tw8th�s.���Uo���~�+���d��
�ud-5��m�P{֘0���|���������I�= �!���#���-l���	��/��r��~ĥ�=���N��t,]=��&��"R��|�A^�%EL�����j֪|	���1�D][;թ����*�k��׸�j�9���wG�D�E�
m8�%g����$�MLC�2��PՕ�XF
�Թ�f[VnK�g���#b���/�΋V'��|�S]����S�e��Z04�
�e��ᣣQ������?8�W�S+�(���M
.3
�'N�,�pH�%���O��UY/8������I��m��~pۙNF��FC�|+<�����^��qf�5:�6O?8?��2	����o������p����cKX(ޓ&l�7}8󕅁He��,����Zg��^8|�:vz���Q������R�[c�k��1n]�����a$1�)0F��]A*I�����eD�lY���<�q��پԀ�sP2�h�B�L䬽v[�mX*W8�
T|bg]ʦ��"� �nNH��
������{�+3�<q-�h���m>���p��1��̜%��T_��B���}DHl�i��D�y��*"������˻�K2��"��~�H~�½k���|��J"�'�W�`ӎ~��f���$��p�y�E�h[fg��BV�9l�Q�B���'C�~1��I��|����}=�c�͗v9�Z`_��h�'Na2|���);�1Rr���H(��1�S��8�ҕ8�b�ֱ��]�l��i��n8�l�b��,G���Q!O���>���v�z���Q�@ᨒ1�^�F�!F�!�����ν�z.�U4sJ`��*C�5��ܟų���h{���&����	��!~��Ƒ�ju��铜7	�**���ȿ�,�{���Դ��M{dY2��E}�|���W�&����Րv�l*y
؜-~�֬�������EJ�i i��O��=����נ�Y���[��H�P��,��������+Os�{f��?9�y�8KE��a����J�[��ϓ�D��	W�C1�I"�4�VT5��X�����m������TH���,p	��z�=�mC���9��46�������͕D�J������ ���P[t�5�c��_�Q�����,�*ڤ��l�j�8�zJ�����x����Yu� /��N����Ͳ� �Uf�7foD�3��8yE�l�1���HN�XS�6�x�����2�����a?�p)����t_��S�!�������5���{���)��+-�|�����5́����|!v!?k���Ħ�%Bd�Y.�SSg���%%DL�w�k�'�%��1Ex�]4�
\_=�6�K�.:N���ǣ�_n��ǆ_[!MEl�fH�xog)�b#�jҿ�S�$��I}�k���~jɭ��Vq)����/�QaOg[޳���	�29�<�{n$�_�����Jd���&=$�J�K��SNms��J�f�����m"�>�ľ~C��!LD;��o��*l�H�l͞����xZ��qI�����.��lF�⯁o�hQά{�����J:�ۣ��Γ$+�@�&[Ҽ7�S}ig�P��;r]�������w�}c������`�e�����ݗ"�i3yR{��r1?ek���.0�����+�/�^p4hs�-{t�E7#G#ۦ��hJ��[�$�q�U˖�C��R�)�s�����Q�)�q;xk���'�slb��N,v�Z$͡S3���GI����ZW��|���ҵ�3Er���R�{���런Y}��F���7�(UgH?���SZ9�i}ok�1�@Ć���W,��9�J�J�@$��t�ЍOj�C�'��a���S�o���sK��Y��)��L�f|f�ze0�d��s�s u�S~ܭ��SL�h7����Øg�&���Dg3����x�G3��*)�%��|bd��xH'��yf��ɀ���fV�����߀�j\��5���LF��70�pQ=�'x�����L�����$�3���o. r���VOg�H8)�L�aX����&���I	��Ŷ�������l���J�+>�x7����Y�'��%��ʯm�ڏǹ7B�K��nG��D�T�SsU1�����K�nJ%�a�;��j]H�|:��l&�@����Ֆ�Tt頷l�wqp�pL���	l��=,ګ1'5>��O��єQ��h��Y$
�ɩ����N���_����>[��Gn7��?�Z��}��˚�����-���u�D2
!X�ۊJc;Gꆑ(���� ��o�O%��NIn��e����2ٟ�Q�u�M�5�2d�05��''�n����D�%�=��v� ��V�c;�)�y㔶�wb1|����\���,;VT����+yNׂN�WUث�M;3N\8�ݦ���|0��L���(FPzx�m�Y���Y2P�%����F���XR(�A7q�]8�#{a*[��Aw�v��X0Cq6�갑;O����Yj���N�JEp�d�1�)�Tοe�=����ѵ��0���I+�����0��C1��rKn���
��� m�-
j��˕��Voe>�&�&��%>���/*��O��1jg���v�����T��D��T=r�b���l�ޕ�&�����8Xoag�N����#�B�Yܯ��u�E������쑤�S�������7dUA@�v��'	�^ޑ����)���y~�W8�q��E�G8�|vW��=�i}7>Rϧ ���n�b�����}���Z�|�R���UҜak�I�B֚*�]�r��/��gtm�>r/x��m���+�
Y��C��&( ��(@����ش��w�o����-֤T)*=�����"	����P��;�{��?�׷{�i��>����Pzr��h��e j�����UkL������M}J��u�4yM��~:"����-f(�Jӈ�����N��c���+Y�燠�*���.���Z�L����ǂ8?�=!98��Mp1l�HO�Q��L #m�˛s�{\�;YCP�Fy��{./%�ز���'.d<���Ӳ�bյg�q�������j��]	\�[8�=�o��X��i��>񾬘��ڰ�݇��|��}�J�Ƅ���myU�$���g��wm&��7�b{I_�x0�uYe�<��v��̅XQ	���/��u:}�vҙ���͜Q���~B,�4b��,c�4�g���v�שk���f��6���{�,�N���Z�1�k���5���6U���"��6i�.N s�0!Cȥy�f��G�uJĽY�Oy����Y|E��(�#+�R�z��,��Q�hͦz�~Z��!��=�hP��9W$�c�Zŏ���FB�}�'oV�8R|�f�l�Ԑ�3Q��H��%�x"��7C�ݏ����}}#�7�2+����~��䱂34��O�[�������"҂rɽ-�_�Nyu�ݥB	>�� �8��N��Ĉee�K���)��,W���Q(r����{�;�5�,E�,Z��1�En ���6�������-b/���@�-��Gs1�/�S+�� ��ZqehX7�И�L[��ň$'#��ޓW�.�^�K{ʏ�O��������~/����	�}��^�;?(?�;�%�Dd�H���,��kmR/x�#������E�1\�ϴ�VX�l韄��3��$9�5ӱ��Y�p�L��f�KV�����ڏ�j;k���r�G���l6�jG�41>�~�ݘɚ�~�G}�܌���L�V؂%Y�r�aSid�S1v�E��]s�__��-��k�Q���k�:.3��d�����w����:C�Cu��z�UB��$�#:��'���͂�+{{�]�_n���浛S�n�ܻ��;�yJ/���Y�8�E�b2m��������nYddQŅꂌ���t)c*5s��2/�)Ϊ�q������p2�L��Y 6Ք�a�C`Ā5���_ku!����-��s G><Ie���Ũ.M�󒗉�G�Ÿ�ѻaI�y��ŌS@UL.��S�aJ�v���Z�Y����<�����D{�"a��������z/���#f�I6�����{Uaj�ԥX)䝡gˬ���v�+C�^T�/�-�n�g^���^q�]Yk��R��cS���Ų��43���G�0<z��@�3���~�!l�B��X��3����_sK�怎��� ��_�3.�ᑽRY4��ϲa�J�3������2�������"���*���AR�x�h�4G���G�ղ5��(/�e���s�[�J<_&t�H��4MsA�ڸ��.]bXwMt�YhW���n1
�Nt}��rv��u4����yJ�Q�9�M�Pp����Z�jԵ֑�fִ���%߹��11.�N��ܻ�?�r��v�|�.�'�������3�����S	m����2'?�c���ݕ蔟��B	<'�b�����x�	���QUI�&&䘿J i��eO��w�I���!(Zi�^���P�W=��l��j�,�^��"-��O+�)�J�x1����������Bb_���R�E�g��P������Oz!����� l��P��#�&����vl��r6j��u,�A �����-n&�f�$�n�}/eŌ�$�K2���)�<b�a���W֮�����id�*��
�.��)�jaU�N�����,j�✺Ԁ%�uk�ZmR��K㙗#9Jь���%�K�k�8m�yǆ��5l�_���H���x*O˄�������b�����ٳAXuL�Z�~�g���������M4�k:��B���.�)�-�+�<�\�Z���V���I��#A�1s�R�����G|�m�Rت�h>A����J��p�?�&�H��l\����'�����R��i1OS�F�LY���TƚL�S����6�
endstream
endobj
123 0 obj <<
/Length1 1925
/Length2 9996
/Length3 0
/Length 11225     
/Filter /FlateDecode
>>
stream
xڍ�T��6,�twJw�tJw�00�C� !%��(�ݠ"�H���Hw7���9�����5k=��k���}?�L:z\r �-X	Cp�q�4�L� ��ܼ��8������jvs��a��a���A 1E�N�y@| >aq>q^^ ?/��߆p7q���'����a`wV������L��+������ �v� m` M���h���0��B�K: .�<<^^^�6���p7{iN�� xv�y�A���l��vƍ�
�w�������^6n` �B�`�;�����z� m0�Oc�?8�����O������p���.60�`���J�o'��mhu�#�m<m P[���� ��t6��j��qA�s�C��[��9�g0���C����O�"������:��^0��;d��	�����VU��	���ك !^1aa!^ � �:������C��Fv��w�!� @���??wO0 ����O�K8|| � ؂�!0��#a�ݟ2��� � 3^$�� ����Y ��à>���q�<J���*�O�����<����/���	
DDD ���������x���
�����9���������3h��\����9�/���^�?\��x�;�����[��������Gm����e�����p�r������*k�A��ժ"l��!�GR��O��W�O���t �ßD��(�9�X��}� �xy�G��:��Vq�uV`�R�w�g0 �{����6nn6>8�H��	���k
{��o 7�@� �= ��n8��UX�#��S�(�+�x��D���H�� ���J �%A ������I��������ɏ�5i���E��ڸ��Lm�fC�v�i'����  �;{@�����o����9id��wοA�s�z@<m�`�����ƽ!��1I�q~d���?`7��*������o��<P���@��?0����Ґ������@�Cm��-��lB^п���͈�q� I�#C���sG o���"�C<0��-����b������KIſ�?.~0�ę��%BkB�/��h��~b.,�DƛtG!Of�9h`�Ss���S��ՙ*��}�w1�m��>x��J&����������v{0Nc?,�6r�x?A����݌-�h�&L�C`�b-��}�Tl�(8#bzC�x-�Ѳ�m�Ĳ��31'�%q3K��W5�E��Ğ�7�P_���Ŧ���}{�����Q�zg�py8)5��SFb5y�{�F�����S�Q�;�?�G������9�G���^K!W;p����.� �͢bglD��eSN.y��uD�h���Uy߫��g$�vC��h�J�z�ϒ�����3��v�TG w����Y�r���E��x��������AM◬���]�*�Q"��Ћ6:N�]su����R�ݖ_mL`?�J����N)���S����"n��`�oh��gܒ�2����|a�QcxU֠����3����ў��sj��ӹ�=���|�|��_�6��TU�)/sȸ��S����v�my�p��]��-����h����b�p��1e ;V�Wå&�{��� �Z��d�n� �q��������?P�m�}���vZ#D��*��_G�\�R��n=�{@������p��t��=?}�Y�ǔ>��_d]��qZ4��W�}���҂:ݱ��Gp�!]F
���m�dћ�Cf�4�Qu�h�[�����/$��	d�X�W�;��$��v:i��ح�{e6U��6LC��<��:ō#+�Y^a��_��[���&5G�k�=0cP1�Tۖ�c��&�,K��Y����3qiMSv ~��S���U�s����Ts�d�ϝ����|�Bs������= ٤�ak��ez�3+�i,��>|�g�mj}���)�`�4�A(� ������Cd��R�C�%)qt��(�Q��P����I�X�_٪���q
m����t�s-�>�C=����6ȨQ��2�G7��2j�-��~��/t3�A�*V����$�BI5�"tna��CY���M�MTE����*�B|�
�I����?p��ٺ�h�O�}���Mc�U�7\m�y�>0(�ΈMR�j�!-�}����P<�i�{���r�yVW�JPބ:�28��A#���Q�����3�M�������2��EA�S/�|��/CD�M�Z�U�:^��g0a�B{���F�dNI�n-�ĺ�Шé�ÌV���<9FZ�$���A��~����4�%FȢ�70�T-0%r-��Q�F�Uz���h��HL����H�U̟bj���h�ۡ�b�K�4�x�?gAw9���D�i��ZOe�p|{�	β�5Ȉ��k���ZuZVt�i����G��< M���n��Yh�H�Q�hD�a���]w猥��
_��>���c�>��6Q'�@iO����o�s�	.����X���T��t2��[�nSZ,xA��|���+�I��kU�eڣ� O���`�C�Q󢣅E���[sǶU}���'�<��}ϋ�ݖv|Z�|{ݏ���h`(x�
��@c�ʄ��IS1�);�,[��ً��_x^$$�Bzg����Ʒ��]c�\yy�Џ-:F/�Ԋ��T�OT��{���M����
"U.:9Vj�Ս�x�0m�ћ<r�zQ�$�_����@/	s�>��~k�ćwO���_n�1#�I�#/��´Ɔ���3�9��J��Oi��L|Ԁm�0^�msHA�RKm/����S�u�LnU ��v�ä��R/]Ō;�Sƞt�ؠځMI,P'z�����'ҹ�mOz^�f�f鄢�~T��h[?:}���ɫ�W�4l������,�(9!��j3�� �Iz�t�!5�7�ψ�ݟ�Ҵr7&�(���e)D��SVj��� w��{���ˆx����BrN�'"0�N���V�E�� vT�m�ܢR���˜���=�(�l=�D��䗨�EC��,RȻ	�[�����V�v�����;�y#-����[�3[��d��B�ae ��=�wR��Y��C�a�Dl�֜ƬD,�����7,�xe�V$��J��Hg
�#s7��I��ۉ#$Z�<�x4xꜛ���9��Ab�2ʾW���.ݓp|-'Q\VA�w��W�y��	/�^��u��T�����7m��*As������z?����31Th.}�zZ:�Fj���`1�i�H):�|8�^`��*��\ �W�^V��#�j=a%�������xu�!c`:�=Gͷ;�C����TN݇8�.P����Խa/�#�ī�� ��U�sFZD�:���i�;����O��
��r�q�5��J�}�_�m�$u�(2\�k�J�_T�v�`�C���

ߠP��Q��j,{@j��ߣ�A��"Ԝ9�U<b�r���@�,�16pnWk�{g�r���І�U����i�}��0�#� lih�U���}�d-͊���+��F;"rZ�K?F� �G�Ǝ�B鱓U�Dlr��7�W�I��P���<��p7N��M��\�4�u˟w�^��_�R�
Δ��gu�e&��i���2�|��f�ً�j[P�zh\-N9�eo�OⷾPJ� �%�gCj���e�Պ�z���N]1}�JR����%���e1�edܡ #��^a�)6���}�5��F��;��z� � �+7�Գ/l�J���������L�ٻ��i�1��L ׎���˫M:��N�<9nBspQ}�C��A���g_���Wm(����$��PNp�s�ZB��W�����������%��^��y��-�.����{Y�Uk.��e��n�pK}�@g
��lSJ�)9S�a�v ��f-)�,c��͗
|�r�T	�Ot1�LO�ּ�F�Һ�+��SKUEE7,dq��G�%��,?���ǈ�lV"*��[2wA�r���d������ �Mvܓ[���4�����eM���)�*摣&u0�)3�J{�Jn��~�{dݫ�N�˞s��Hx֫��}���EH��Vh``43��������#�	ONoʆ��t`|�F0p
|��k���Ÿ��:B�����5��]�iW������q�U�XA�2�j_3�?���Oߒ�t69/�{K�.z���J�XS&݉VLxy`��q�����+˱��|ȣ�
��Y`���R��/{�T�	�氟��Uw�S�Ż'�R%g���g�5$m[�E�v���-�d� 9f"d�Q5��˷[��ߦ�(�6ǫM^0e/����@7j���$=���9�an�}]�)�߯U4�$�w2"�P��&Q�uuwǣ���6�6�.������U�9��r;���'o4��CX��ށG7!�� lA�SZS���R�hl��5^!"�� d�gB8_b�$y���]�Y��t11~sN��,pz�@W�v擄� �*��z$~������~q���=�iv/5l	��Z��r�>+��XC1�g~�{f�`b����Q@p��Ĝ� D����d�h%$X;(`�f8�1T��!���/�*��%o��ݛ��Op�s�3rQ��$�7ӓ#7m?�Fz�,������3�>��*k�\ȼKKo��]�Nn�/}���}�>�.,Sw��.f\>LŸ�����'j4�&9�(8][[��I��ƽ+������{�h��=�;��o��,��"@*{1�z��,h��*�ia�O�?>r�t�R��-BWO�4�76"��������/&��l&���>��� �Ey\z�c�'�(o��Y��!.��1~���6m���NR}KS��NF��	��ˬ8����= >g<�� ��"�����Z��x|T�4�SҬ��������	I zX��H�LZ�$���%M�shh#�=�e錭�vUx�x�-kP��e���^X���=n�r,���#?!_�k��x[��ۇ*�*�}d[7a�q����U{�������O�Ϻ��Xx8݌F���Rس����9������&�R�:3�Wz%}&��ëv��S8����Y�]~�u���`�Ce�F�O�%k��l'����Yn��GH��Y�|F���e�W�1�]�z��`xxx}q}�	�q�(�`���߼���V%j0����D�2)�F�,	���!�KHG��k=o��y@9���Q.��NF镳�|U]��c�@��$�5h._|�2k�{Js�V3����d��G�C��]�s���C�v�4L,�G5a[�TU��H�f��8������W�� ��!n���(1�C�&I����������K�FTı��Y�S���G�r�'ѣ����M�6�W���þzke�Q��*pi�N8K�|��S!Y��`�2!�B��T�� �C�[-C�6��1�p�e��\Ǔ>��N窔���]�?��u"�2��s(OS �ú���j�R�I[h��3����d�o��F-9��e6�>��O��D���=�����>�#�3n����&�g���3ߦD�s)�GG۞d灖F5��C]&;��;1�v�DQ��¤#�#[m_�cs+1/�X�j�|�O������>��=+���ߕ�ѯ܈�֢���A%�n8�Q�����Y��/�n_�|��u�+�fM%Z�l�����}y���1����k��zU���a���3vZJ�,��}h���Um�<��Jt���6Th�.]f�?.�Ot�kH�POӳ������0]@��t�p ���i�?4�>�� O�2[F�u�]X�y�S�6�F�0j�g�9 ��{tɂ����îri�X��q?��k|�i8��Wa�7�����bi�x�x7�L��(iӭ:�16s�/�e;.��u�KNz�R�D���F�3�,�,�.Rm�6b6�s�-��#���� ��e+�b�D
�=����.���Nt��ש�|�o,U?bH۱�E*<��vW�%�k@�mQXX��:��J1���"&�g��1��V*ռ�pl��v^�do��ٽe��7��\{K(������@��?s�ib�K%d{]������U���_/�/�!�v�M��R >�»�&�<7&�2q�T����\�t����o,�TΤ b���1��0����b��J	��:�<.<�!��]��������;�!ҔCS��4h�]N�3</���!��=��)�7�ȯoq2�E9]j�q	�6(�Ϛ,�<
��ޕ P�J٤�]�rt'����=�"E-���nM�z��Ͳ�7�tZ�K�j��3)���x���88���s&h��[����
��,�����s����lT���P&���NJָ�=vN��ű!L�,����7D[�CX���*���f�|��M����3�lk�E�On��V�3Hrd�"$c~Ŝ%�c䝿;ո���N}|��V�,�Cq� >�����iMI�H������¬�3������ l7GPЬlsʡ����|D�dݻf;Pu�6�u�z��MW�/�Z$Bʧ�	�-/���G?*F�� �;�����%��;Ul'^ܺYH��Ņr$�"�u��>�Oo�P��b��ʢ�hq�V�~��E1�Bر-�o�U�P�mn�q�M�HS+��)8N���j�	�pL��M:�x�������y���yc���������>{CO�"��J�c�i�=��A8\���}��z�������sI��%�,�f1�e��O��0�;1�-�����l�����z�&�5)�g)��tx�Q�V}��(Ʀ(������dG�@�U`�i=2p��ᡭk��	�qR��AITp)��d<-*��)��JH�{���B!���s
�JRvOc�I$e%����)�a�������-�IA�%ȑ�Z�]`_�_9 �

 d(;�����꧝�{(�>�Xޮ]~���ے�x�dݹ8�eb�g���ǚ_΃�w�}e�[>(ΆhB�_x�CJeF:-�6�a*��E��>������h�.��>��(1!w;	
����
�y�d���w4�d^�UGEb��F�M�G~���� ʯ�&��As�j1M�80��M7A{l�W��G�V��ѵ�KL�R��LG|�Mʓ��TkTnn�P�`��A��������i]��j�dT��mÄ(=��'04o�nb��8ܔ/���[�A)��1u�h�_-D��I�(.Z�����h��.3��Z0R�;g=���*�jk���`Q��"�+�����_H���6��Rb������]��K�-&�y�g߆�Xn7���'�f���,��<a���.zu�΅v�Щ�A��C�
��O~ij��g`�f�mǼ��<k�Z�g���Y��&'Z�Y�`��Ѯ��=���ME^wc���9����M�T�Ny�W����m#̱��LޕD�A��2f���BqIW�]gX�$��OV@]A+$4~�U�u��R/�>W{����GJ��۵��	�;|��&�w�ý������!'�{P�z%"x��D�E�U�|�p�Hk�:�Ѭ�~o�F(�ˋ�2��"7�WS?(�7���ԹU��5?��[뚖�7��U�;l~�~	y��C߽e�̧ٛ��j-�����p�j�����0 �8W�8��z�A���/S��-��$��0�唆��5��O��Y+�w��Z�����̉���5yo3�a�8DO��r�ZBOs�W�X@��_�x'��R�W���ۙ~���?a�O�1���"~j��V���k���R����+���6@��{廪�y��6���<��P&�aN��]�^�f=`!���$¸�)ˬ�ʩ
$$M_�P��=�'�X��J�Mj�Q��_�9ն� ���)(�%��_��#_n�"H k�߈
�6in;�&�්v0��R���}V����넮)��tH�_�����i�����r��+��w��X-U_&�P��q�&�{*�!}Q���T9g߰2ۧE�Ye�g~�c%�bQU����64!�y����ͬ00TFG{;�<�$�jPL�uw*|�`.:���yJ]u�L���p hW@��0�9ho��fU��G�]E��A6����ꛚ2��b�������U��%u�0)v!����'R��t���X�^����Q�쳚�Ea\c�6K��a�M�'޲��1�V�o�ٛK�R�ј����M���Zk�
��f��f���I�zk�[i�ӰӨ'n7�M'�6�q���h�^�%O��o7�붾*&�4�o[�����<!��ZPb�,�o(t<!`.s��xd��{�@�Р��5���r`o�f�=r�45�����#��!�x����p�͸�t�'���x���'�{𿲡��?]��8<!�w}��X�E�������=�������J�,�%�1��${�������<��Ŏ�ݔQ:W6+3/�M��ҵQ�V�:����Gu?r�s��)^+��/��*�hh��]�+�<� ��X�[Ky,�ܮٰK������tQ��^������sVS���WqނB�h����o��y�s��>l45Z��l#����z^`X�=��$s���Ј�x��8�%0E$�f�HΑ��h�!fg0Ҽ�@��Fif(�|X�)<T�&]�/��O,�W5W]���o�#��e1���5o�C�L��Qp�� �r�?8��:�I�U�7��pj�9�����Qq|�LMs���GyTE�N�\���}�'`p�"=�f5�Q�(Z\t�M������,&A�dL��Χтc'��a�^s�����W���t�XKAԝw�#�5Qs�z8
�J��� fĎ���}�!L�;*4�g��4�'O���#oUw�d������o�>���:�$�Q��������,3=��̎����^��'���0��i��x�u?��	7�S�=���`r����`n�����H��<ɾ�����v|����6�t��O��\61��F�9흃	vK/^��?"�~1d�����8�)��K�{)╢.���X`-��z.�`�2���L��li&�`�Q���o�3����V�s��6����XŘFmzk��Teov00�%>�/��<ݿM�dr�(��'t�EE¥�����7�R�w�q����T:y�ZJ��~6�?j���)xF$oS�&���=�>����ۮ#a�!��cgG���ĢH�����y[��OڽFkC� ���G�$ޓM������T>�sɶ&���rB2<����������e�W�����a�oG����DC�S5���9�BEE'��d�R�z�8���ᷮ��g=����L�����M�p��}��U/��r�@K��q��)j7�b�q�x��UR�U�Ό]R��7(��Fo�9���w�9�2�6�f������խ���1=�ѣ�����Y�v{�ȋ�:���1�Ĺ#��5�tܴa%u�E<�����N)*��|b!{�mW��^�������OIz�m�\�b>�J�k$��lvΔ5R�~l�o����kV~�']/{����E�`��S�s�Px����<���ۀ9pQXZ&����#�� ��x3u�WZ�7��4��Ƌ��Ѩ�N� ����=s{C#�r�E׾�������南A�ǴH� f��k����\�k�}&j�Y^��]}6e�q�̓����{��ԑ� �u=T��a���a���B��S���:���qb��/�LC��\Z�pi�ޓ��b�������
��Ig�d���N0*���
H���,ή�Po���g����&��W*������]��_��V�
�4\N�_Y���ܩ�C�[̚�;��a�mÀ�買�
^��:'"��&36b�ݘ��O9�wWu�����֣�Hs�tq�l�gG�,�Z�I���vy`�C��ϊ�kN���E��r��/����%H&�|�v
7� ��7#�^��jLA�]�">��1�Ȧ/o�/N8�t��N�+����*��δw�2����a1f����v{W�w�u���6&�����T�3�(���{ؐUdp_�lg�L�	��)?s�Zp/b��CHhwZ�X������ȥ����_��ǲ�J1��}�n�3�쳫��!P3%y�/߱��� XZIc8���TU��)&�)�{���s6�I�g���nk)�d���>[Xw�Wjy�Si��["��d��1�O\��՛�ҋ�	
r
�`G�֪��F�G@[��/i{1�1CX���>����D3q�`��Z�f0�N~i�����k1�iP	�Q��J���Fm�t�Z
'�X#�O� x�Q����,Yo��<I��"c�w��~H�eP�ST�[� ���5~��k_wZ̺������E;WU�`��G��넰ƴ�H���8�oX0�bUx�:�QI_t�gr�҉�X��V���,/� ��Ln��LH�C�2��T-�x��Ι�v����!W�VK�r���W�|Z͙@���\B~�p�bʇX�8�<:R5��ߺIi��	&�zn�/�N���s	�^�]��E�)�ܷJݲ���푩ZiaR�m�\��	��#������W�X�� = ��dQ�-�]'��|7��Nԟt>�?�9N}��_�i=v$��{�U�og
d�����(������=E�����B_���n`�y�u�!������]�-�P[�@2�?d�A�р:m��\{WOl�5��W�ouT�5�0�9ì�5݃�y��&����34$����RBש5�e��������9�1�C
ʕ��,�,���;n�4�>SvօP�b�H�~���'�;��0�TԹh��l�V]�Z꘹����s� >Sc�6�#��kqbv@�Zs�Ҧ_��j����7x����iaG챡O�a8�8ӎD�.�7{C�I�YI=JKuhP|^��~�"F����ò�"�B�g��L8�o�r���헲jG�Z����� @�}��Д�ʺ�����E�T@`H��_�'|����m֣�Jr?9���l3#�1��N�t4BC�����7|�%G�΢��5�D�-�*���VM��
endstream
endobj
125 0 obj <<
/Length1 1450
/Length2 6906
/Length3 0
/Length 7882      
/Filter /FlateDecode
>>
stream
xڍ�T�k6,݊�4"�3��]Cw0�3t�t��� HK�twH	H	ҩ|���=����}k�z�޽�{_�ff��喱v��):#P�@>Q���D�������gfփ�a��`nH�3B����`PԭN���SwF T�@~ PH�����D�vtv�C=�� u��3��g�sv�v��ڡn��}�Y��""`��� '��
� �CQv0�ۊVPG������W
6q;�E����ӓ��qv��d�x�Qv ����Ѐ:�� ��g�������6(O�p�p�[���w�5�p[�Qh�����8p�� ���tE�JG��ZY9;�@�p�-��h*��P\ (���#��|�������;�e��[��CZ��]PH$��D�_inoYa-���C����������n�ݛ��dΞ߿8���kw^}������V��������	�� �+ �ee��+�����K}������`s�������"�0 �����߆K�@ �n�X�l���ߪa6����� &|���~��szrK/kg���?��˫.��b����l���^ _n~A 7H� 
���������V
��9�2B6� �?n/�oт���a�����-�a ��o�'�gu��?/���?�������6�����������1C����9�R�u��ηˁ�_WC؟UV�Y�ݝ��
AAo�Ca{Kqn� ��=���Yk�QVv���(nk8�0-g$�׋s���?�ۭ�r�}U���m��.տ�* ���mHP us�z����V�o������ ^�3�6p��`���k�"` /�����^��� ^��� ^�D�3��������n�7sn��[��:�`^0+��g+�0����24��_FAD��ѧ���
T��!r��'��1�2�S1��$;�z�|f���bD��4c�X�q���H\� ��A�mm�-�ֶt�=O�'�)�\��Lgl)��4����;�a���!D�����'Dk��h)�����j�<BK��%�)n����W{狹��+i4��D.��ے�_;��U-����
����BN�Â�,�úRLb����c���8��5aX9�q�'�x��}6t���t�@�LOa�V��w�(A"GO�h��/�d����t2�qÒ�v����筀�r*�Rb����w���>L�fx��7\�Dʃ���-�Z�� dr|PJ�d�I�kvn����kZ��P��b�:)�--��y���$����U�o�')�j�;�XY[�<k������TWo	d�cEE(;i�<�}W&*�*0{�)�q0�w"��i!���(��	_�"���.�o�!�G���i�d�V�\ir�xx��:��M���4�6�~��JY٧�L���h���t�>���Ə2���f�枘�O�M�,z�\oHs]C�g3�������
f�#�)/c�cL '4�@��%s\�T�)�eG3[���Q52!x�����F;\\�T g��8;%��S��/4����;���=�U_3����v�Z#� Ǵ=>^ߙ� v��*��5��J8��m4#"�|�~�s�S�3���>A_@��
���í�{R��Eѭ٤�v�$�yKBX*K��&�B�$����$?pj��qi����O�x�Y�ci�濩�,�@Sv�V�v��Lyk��\�c]Q��e��6Y<��/u� ��ᑌP>�l��]��Q]@��wU�_��O��զ��ey�ޙ�ꨧ�ܧ__��v��Nٺq�+Y<}$�}A6��	[]�a�yͣ'�� ʈ!��Qn��bn,�����7��5�f�2�d�K�{"�~����#�e�q��i�?�b6��M�A�C?�|��E����5ј8��iʝ����׶���ۊ.=;rf�ÿ��R6o\�XW{"�����w���1�U`!�z3�^�wHk�����k=@�1�n��{�ψĶ����K��z{2�[^��o�_�9牡)zL`�w��ԉ�2��kW�`�V��"��Z,���Ε�b�VQ�'��b�%X�ڊ}���d��i�|�3�u'%7^ي�Ĩeirbb<��Ki|��1�J$x�J����B����+��;!��qIt/�X��]F
,��Etl�^%5�O�K��ĘD�����,M\%w���?ZRf}�=>/�(N��6p��Є�5y�aI]9�P⡍�רuh{��^y�fN$IO�n��,'�W&)G*�甍d��9�4��qGh����~��v��o���Ì����ި�)��.�f8h���)P�b��K��`�y�A�W(h�u\\6˽K�Ps&\Y2Ֆ�l�HL������c��0�P��Q�a��l�].t@��s�ݛ��E��5�!�6��o�H|q�L�A��-I����g}KkJ-�u���0�r8A�r5�����������t�)�;���hk�;zY���6�#�t��"ͫ.�Z�~k^�S
"O�w�6��n��FO��WGv���qpw��T�}M-����z�s�r��Qó���Ml�r`� W�ݫi�F	�B�s�\����_iȟb�e
aj��Ujps˱H���l�3,Ԍ�*+�r��M��dټ1V�5���1k���/,�h�ǹw��T�gY���GCa�Q�k�>ވ�iw��8�ԃ2Gp��Gn-���$M��G=�z������	�m����P�4��@�:��*�}���~(�ܕ	�i�%���b�z�-�B����ϖ����$7"�5bK)�1Vm\��ӑz�s����ǋ�}�L�G�����?�-����Qȴvց��]n�u17d���GF=#	|����E���%.>N�A|����_~�(4������3�ރ}��>&��wV�\�_kb�uW���w�U�
s�.d�p#ծ���ʛCn�eȕ�}X�Ά7Y�^��ƿ@{>���Tp�&�<�͛�
���7�ߛo�R�[���wS둄Sz����1�? �~/!��Ӿ�� �ӳ��k��!�ed�ÍT���y���$�����P�[i��e���H��B��������7~S��+�jC��;H��cM�i�ם�4��A-7�'<>�aqY�9c�&��ޚf�n���CH���	4B-�!��7����IX�8r�mf6�d�;�L�޽{��q �Q<'!8�?��Ӭ�L�IS��ب�'��)&/Kޡ�=���������I�J��cj��bQ
��e�O�T�}#���^�V~/zV�Yx�c�f�����[i��*@���d"�H*��B��{����ZoeA����"=x����+3�w���!������<133 �����0riCv��	\�D�ưZ�dZ�&���'���d�}C�Xu�L`y[(u�$�^�����g�b��{���2Q���=���OPm���0M&6��n�@\u?�-O�d�
�d����G�ԹhJvO��� -�B���6RT��z�;�o����_��z,�3�G�i��˓]��]4�)ھ$%X�%*�' ��,z4����n�r�0�am�����Fd��3��B�p��]����h�,\?}��S"ы��$_���Ɖ,o)�"M1�n�q�X"r� �=���}���������ͬ� E��y�:��q^:9�T�W�S����Ap��{�@�xt�pm�s#a]<�zIo:��L�o(��ȧv�V�$Q`��c�
�-
�9^|����$��xS�}Dji��S�z7X�$���֔P��%q�J��1s�ȧ�g��P���It�r(_<E���`>�"�����h�
��}p���>g`/�alr�s�pA��g���m�u���2��\{Q-r=�ߴ��3��f\��m3��6��������H�����/}u����3�i�����;Y/�u{MB���3[��q�ȣ��Q��si�KQ�9IS�k��c�#ȗ����uۄ�s�������۪Hl����|�jl�#����N��'!��o��7A�������'�P��QCŗ�(i��s�󒶔4q�BO�B��)f�&�u�ñ�w�K%٤
���xy,�2�T�'�w�m^d�mfa�yw��;�ĩq{�⃺���REyn-�84֗x'upqZ>��k�b�b�{�.�����0����������
��{.z�v!.?4�\��x�N)��	����&>��z���a��K7+�Y�>���H|�.X�xS�tfZ����F깤�ȁ���iO���� ��k~:m0����	d��^�Z�`��BhrY\^��!�ջ\��w�&�L<��_1�2�ʜo�7���xY;����C���=���>�їxD*'��o8ڎ?Y�J�30V>�˱����Y��l�3����2���6��*S�z��hpV�KI�ټ�+���4�S�=�� wuO�[�K^[a���w����ː�|�:�a��B�#ۀ$�<z��9D�u�L����q�X�u�pm���������q����:& K(Vً�SE�,���1/�u5M�r�DM���R7F�3Y9��>z��n�7f���.2��/���wN�&�&{O:�t��*�x'�Z�H2~�S)K�0�D��g�{�E����a�hgtAl�6T�ܣ�*K�D�7\��T�8�0�i҉��c~�b���Z$l�b����S����P�M��_%�N�����g�q_�#�!��&v�IδW�]��6d!���2��S����<�_�9X�O]��0<-�UQ����<�?y���|G`���k���#�*��Ejm`���0]�qCId�E8�v�4l;���E���aI>_'t:��,��hwE1��F�%ڏ��M��3�s�"fp�Sf
�Φ�r��B���2]���r5й��I!�������#7�h�#1�q��S.C�M�9� �2m�kI��kl8�6Ռ�+અ���sP%�����S�@]�_k[�~ٞ-�.;`(G{(�C���Au\Z����<U���p>�i��oXm�vC�[{���i�~RPl��X�\F����GC���6z����������u�����;R�e�����v	E��^�|�3}���iI���:^y@c�	�<�`�}��[��):@�yh����%�+�J�ׅ��[=�{ ��Ha��we�
W�_���b	9/�^x:y��X����s�J�KV�p+��B+!�>��'])��)��	yyk5�T�I�H�lI����[�[7�����[4eG^��+-J˚��Vj��	-���f���!"6�j�LQ�[�y�Z�]��˨��/-R�s0%K؃I���J����G?p���,Q��+�6�c�~qG[6���	a��q�J���[�{���B�Q��n�r\y ��򨖺]L%??��%4&�u7:B�~��_�v�?�˸D@�sD�?*';�8}>O�2y�,��^YJ�k�+Z��4p�z����uO��U��,�N�3j������=��1�%������<r�����&�c�٧�dVxH]�sw"����^?+���� Z쯤���ǫ���X�7X���d��y�H����mYPީpƑ2�J���Kjq�;>lm�a�F���|�q�ѓ%S�Ҵ�!����g����%?�ɠ�X���;{�XvX��Q-��C��r%X �r��])��Ӭ�.��y���ʜcb0s�d�'ݸ��u��v�����	^��'�y�b�Z�JW�{�އS<����dxƐW�[bc�E=��Z��������F�K����ʵ��$�s,��	����8�4q�qyg�v?7V�}8��f��ը�k�΀]{ �ܴ`�qR����;�����Z�k�/���{���^��Mn��C��;�~d�$�b�x�L��Q ;�T!J_m0��>�ŀK�f�J��'�N����X��pS�,74�bi�F����`�g͙�a�j2�΂���k�27r�g��V��v@��[�Q<�o(��P�/�]_��8���H�1:�[�(�:�݅${Hx����-�a�N�H��c�n��˪28��ۺ\�"��e�+*�{���,����)p5Uj�J��]7���`����u���ĦuZ�cC/�P�C��S��!�'�gR�SQT�D��8r�h|b\�����өr���Ӎkͪ^P�X@:zȟ��M��bH��-WG=ˮ�|7��a�-�Y�mYyT�Hh*�J���Yp��(>uv�ދ",�g6�ݼT�d�~˙�K���*���c��7}0�蘖ofvf�0��x%h�"���S?��;S�	���^+��c�U)k�7��О�tU��ZO����r�/k���s:�:��w)o�E뷻�7>O�/�pAޛ�}�k��(]x3�2)`��RDB�-��lk�*�c�;:���*j
�B
 5�b!r:/��'ٴ�i�3���
�J�W�����Ӊ��V�d.����Yj���&�}����>a�q�b~�y�2W��Gg܃-�=L��n�(�0`հI9�Ÿ�4a�b�� U>�=Lc���}�y�fUO}�����U������zS���j��`��0A:���l?��>R�T=%�b{���p�����	l�%G����0Գ2e�k!��ej8׀T��t�=�p�<�a�5���	���щb���X�cv,t�w�|��@N*������85�\�9����\�2��]�o�R�������
K�G���^4K�O���<X]D����il���YŠ�JX;'��֒21ܜjk�}q7g�=�iuB��R."���?ݾ��ur�%�r�`F8���F�Q�n�m꾎�s~�o�)"�Hl[�\��ܷ�G%��d�$UջӅ�Ne��*�)��NN��;���
+'���Bp�)�gȺx�$�k/��M�M�ߍ1x*���<g�\=�۩zsW0����M��Ϩ�t��$�o�gd��{�S2�ϗ�G%5
6k��	���!�L�˻(�'����M�U�����tEi�`���/��)��u|�� �s�WX��j?����|�ye�g���s<�kK,�0`N��>������r-�!l�L��
Y�����Do����	�mJ�V�� �N_��]�^�g�E��A�@uc�D�z��c��J��9�(.�j���JohZ�'̈*/���YK��pgJ~��B!��Í�nخ��&5M�ϐSI}Q�JǨ�c3�˖��b��n��Oj�V�=l��<��=y}�ǩ�%���q���7�&I�M�����7u|���� �Ԭ�g�Rh�;�\f�2��)\z]u�j(D��qFf��hZ��;Z-����k���CP
7�a+A��f���s��sd!�lo<�>^Kt�������T�~�&.��_��p�?%�;Y�g.pl��{�����.�Y6�j��o��2��t���;�Ӹ��R��H$���r�� X��|9���I���x����f� B�e��x��P���Y0/)�;z�C�j��g?4�+\-y
8(p��A��qx$)zs~������_I��� t��O}&x��g�]t�dϤ�ͥ{���O�<����2�!�G��YxV�1k��P��1�>�Wڂ��$f�A�3h�?Qüd���gYwJ�,�^6du�|��r�[ᶆױ��?�!0��������d�	��y`�W�)/���m���#�Ux�"�p)��ݸsY�������EQ��Ȍv��w���>�O����u����3�����mmi�}���������leU�#���.�YW
endstream
endobj
127 0 obj <<
/Length1 1428
/Length2 6392
/Length3 0
/Length 7353      
/Filter /FlateDecode
>>
stream
xڍt4\k۶�N-��h��D��a�0f��-J� :�K��E���[%Z$"z	�Orr����������{��������z�|�v([�

���K��FF� ,��88����aC��QH��堈�A0�:%��O�4�@P���!0X�oGZ
P�x�� m~@����8Q��h��#��߿ �����Ȼ��p(	hC0�0�ˊP0DA�0���Rp�8b0�R����w~��.7/�	�80w�f��@\`!�q F�p���({�'.8�t���"�`h�8`���9k����9@�_�?��D�JG��@�(W��t �����ŏ�����/G�u���K�ߝC y} r	�<w(�q�w�#~A���򔕑v�(��՟�^���_�uF�<�>{8���;���}$�SW��r���s�a Q0,!,�� ��Q�Wz#oW�o�o�%?W�+`	���]~@>��Aca~>���o	$(����� G���~����%_����������Y^���Dx���{���Z�*<!��MA����|B��� &)��;�����V����|�H{ ��ˣ���Rp�Yn��tP�L�\��,
�^�����;����_Y�_���T��o3�o��a����.���\.�6�r5���� ��"k���X����c ��!�t@����*p/��u�M���p�G��P��_7�'���r۠Η����~�`�����H(����	��4�_�KHT��\O;��o^�H�2�D�أР_�US���(��/���%���5Ţї���%��-��`0/�~
�u�	}}�B�ɓomHv�c�A*7��{t�;QwUf�"�H>���j~E��Pn����Kc-QxS�~��u���Z3hf����/�/��In�ɭ�^��9�7�jp�a%(��hO<�T�^v��>�Z�_��$=/㋹mT<��k�5I�v��L|�fǋr��h�&{�'�F<�o+F���lI(�t��r#!�6v3zf�C��QN�O�t�>%Eы������83���;�#�B|��<�{E��8�/�ND�;w���e�h���N�ٻ�j���}ާ�_�����jZ�6��yt���d��ܾ~�	�x��U ��s^g��9����I��U���������mCYwc�evYi�﷞˹���윛N�n=m�9_��%����;.������DxP�yx��kC��O��T�[㫂�����7���>9�-��j�.q�<��O �R�\p�l����uLDB�X��n��t&�q�g�kG-�L�7�
���?b�1���Lu��'&���z��k;nMh����}�:�޻�B4��*o+�mD�v�Fӝ+_�Be��p�oC�wK��JĠ�mT�V����������p�a�(�p����n��";�W�9p8"�>/6�����'����7W?'״W���!���^5�w��r󖂴��U���*�M^���7Ơ���c�o*�;T�B�w�k�2��R'����j>�B���N�GX��	��V�fɖC%�����ѻ��=]��	�b]�3�	)"�����U(�6�#�2��qvԕe��j��W�y>�nQOI0m�VO�R9iʻ�H7�t5��XMx�*�{7G�.�R������a����;��F
=5z�����lo��6�dU�� /K��&�+���[�Q����p�׮����D.�h8�u�'*�s����(�`KI��77��+�B{�ſ�����2��f�B�6�7�Bg
��nE=��z6,�%��ּp���_?��/o���Jy�������[Ea̰�q��ٌ;�+�e��8��;���#�pe�Ļ��=X=���|�l��K�Y�F��" k��Ƴ�	��7�kߥ;�rJ���h���,��!2���c�H�
��ΰ�tJ��N#"I�d�3is�U���~~���z�x��=�~7�x�[��%�g�$]{�N��ע��T\;r:�U�!���[�~� �v�:=�1�-��x��7c&w��	�?��A!���䕷]�Rp�@M���Y)��읪��b��j���a$%�ܴ����t⸰/������2��A4���0���*����3������Q��(@q��D��v�e\�A�z��T�5]�V,���Q��>c����֨d��ݚ��b;�0�];��&O�v��]��	̇0��hY���X�8f��l�o��F�d�>$�abN����=�fH�Z
[g#����1���^��2<�E�}:+����s����{���i�{)q���y.&Hި�,��)�z�x-p���>KN���
g^>����=��7��D,T�Qb�,m<��Dx�*��tw+�U�����(ys\����8/|Y�u������lGk��(/���e�t�]U���[��j��>���u��I��滞N-Ř�	�����p�dީ��B�19���dǱD��;Z��б�^a|�����^[�W`�qe�*��5����C��u�b��G�*�Z4D{6�\L4��%��A���j`��Ӵ���k�B����Ů���z4������^��D`�-��9U��;�M�/{��2Y�G����)��R���z�T(@I70�_�
h�?m'Ilk?c�&	�g- b^X�;Wj}�*��$�b嚝`�� ��,u���tF��Kz�/���'��"J�t�)�28��]�f%���|�9!�օj�^���+s�d��AC�Ÿ��޶b�OzVF
�'SIG '�Wz��z;��>��б��D��-R<�?�F`����~z÷�������>3 ��å�7Ȗ�1�>�[[\�X1��X�~!�<.?�r���[�3A&���(�nw�
n��^�	���Χ�ރ��A&հ�
iQ�V����2�o�r|�np�֜�}�K�u�߱���"�!�@��Ii���for��� �����{�^��=e�@	F�`����mSB��(�V���3��<�enW�#ZG5,�^W/AI��Ģ�1�cy�9Ͼ.��M20�9��#HX�I��·s��R�{�!X}�tN[P�oZ~�?AXj�޷#�[YR�]��ka�w�{@�Y������#������=L�'�\k@�P��V��%��o�6n����oN�>s�=���� ��.~�7��SJ��Dp�piE����x��E���r2���uˬ�����z�I;��2��L^�p�bb����$+��U���#Ĕ��ټ���	��\�|�}�>�7v9����;v0�\��/κ�`iU����({���A�J��,y_[K�O���\�m?{%$����L��bs_Z���M�����9~���zj�� "Rj��^���j솅�nm�/>8V�m"-G�����|�}�x{;ӁBA���X���偨�,�ۣE�˵�+e(
/bܪ�J8%�E?����*ǎ�ӽ��iyr���M� ��6�S*y����j��CYIY_�Ƕi�R>\5[U�̟��ѫ�ex
MLP�1]��f-��ծ}G��Yv3�?����H��_(�����y�*jH���t%��q
˜0�;'9�ҴG��]� ��j��g��>i}BⳲ���Ƕ��T2�Cn��6�p|~��ހ�UL�W]��|�g�Q����ޙ���'r���9*�8�}!�����HY܎j��Č��5�Ȣr�b	F8c����O�a�*����C��BВ���ؗ6�9��4�h�G��� O5�:��D�2�v�A�%fq�}K��B8^���J�G�����اO�m�E֌�ORt�}S����㍣<�vd�a4xw�"dg�Ə�ݐ��f���v��2��T�T�ڈ���}f:s����j��2�Wᴏ��l&fN��N>C�ܥx���A)V�v�Ҵ����%��y�D�d_�[ZI9������c5X<�!qq)�7�Un ��
>�����CG�1�7!�P�ei&}O��aI���l�Zk��}���U�:խ��D������?����p��6Z�*g�E�#����5�Q��=[K��v# N̟Y��#<n)}R�S�[���A����s��Yʰ'�T	a�
�d.<d��u�h}��{�U�I���L+ގ���J�4�(d��x�*�4���s��ǺLWp�4�[�E0�����l�灋t�����8t.�f�Y���DX�E�I�-4���O6p�0Vz��O����r����5W��)c��J�T��(�zJ;@�E�,��_"�I�>�Ŭ���:\2��d�Y���l��[)L����F�jP,��ܿC�x�սLƙʜ��³���E�U�h~�WI�ڣܤ^/�:�{�F��ҹ%+_ș��G}�t�m�T�7F-��oTd�)]{*3Q��t���Dؐa��Q١E�*K ��֛�Fqخ�M�sX��fj�O%�Aߺ_��!��M�����̚+�aF�I�ӥ�	��c���E:m#R�P��/F�X����{��/r��	�Z�Mo��t�WxLy�c[;��A�	����������Zv����w��1u@n��L?�����D8��+l�n'>�_ `"Hf���&��ÑS�_F�z`\�X�z�]�K�����UXf^����)��{61��7F�s�P��x�;8�� �~�ŗ�����8��Qb-܋>�
O�M�o-(v�;����'D��g7�7��)�3e�)�-��J���l	'�iUޤ	��/ H~J���`un�e�'�G�c�-lE�A}w��{���h!9��.��QZ�#A.w��Zy���w���,�j^�@��8�=
&��	�#�	�Ծ��l�|��<�ZtT�zƾ(�a%��{V��ZXHfbX/��Xe��wGd�����{�k��"��$��2�x:?CS1�iQ�;����č�pK�B���yVqUb&����KƑ�:�of~���Z��nDO�3�y�D��{،��P��#LrJ.RW�J�Ht��ء�3'd���W�Y;�x�r��E+�~87΅e�&���/c󃒬�B)|�2ɓfٷ�n�������"2I��;_Y��=�}�5m�dQC&p�������:��;�?����Rd��4i��$=�ܹC��gV�e��n���>���c"�C�C����f�:U;�`�4�Z9͋�K����n���4b쯳�f}�L�:�Xݯ���I����dLL5�F��J#�0�Eb�Ŧ��LBVn��^S�����K�c�z�n���T�Y����vKob M�W$B��%�|��v�Gޟ�K��T|�����D�ot����y��O/���Pf�ѳ�~lw�ɣ�ᨘ��(G����[�BQ�9$�+Ns���1���'�`�|�1lvm�b��Dꐖ㱬G����2o�����)��iܴ�<u�H9F�	Lm�I�R�*Eo�k���St&Y��W�vf_ħ��	�ld4��\��|�$h�hm��P⌀5�fi
o��u��2�a�5����>6"�zB��kǭX;�&�����7���j��V��H�&T�}tn���}��'�}� �Mud0��#[������a��I_�S��
�?�Q��K%6�_`GG(O��88�Y����U�V/+�~��Aˀ~�Nl5S=�~�C�I6ݘw$������s�'ZC�l"��{v6���$�Mn�-�ʝhEE^�	�Xl��	"z<�s��z� ���x[�J�{�%�6�9���{Q�Qv?"\�sm,yZ����3���w�Q�P�#o�jK��:b�������k����"���+*R-�H|���*��?Z��i�N�faj���jg�lZoIꈾ�\�oWL.D�xe(����S�mX����[l�q�
�����
�`?�����<��6��3�V;�@�����'�z~�D;w8�����r����Qx���q4M�[��jE;X�&�0��T�W�i;z�f�q~���fZ�v8���ʧ� �*�H��Z�ĵe�ܻ�غ%�;4��8��mX�g��v>����80%(�������E��Ac"Y�E��}��w�g�T��i���H�ɀ=V��Xhʦ���[B$�d�یe�*�a�V���nL�ml]VÃӜ���8������X�h�7��k��93Ø.Z�k�a?,bґ�ީ��+�E��_z�m�>�ϬR�%X�~����3���SqI҈�6(���lq��k�~�[_����]'�H��ڈv� 2~7My�w�,�b�q��E�;m��+����0ߞB��\����/+�K8`5�oސp��QKu���I*񋓈{ׂM�ˇ�Êe튊�q����cc���b�}Afm�/�O\�=�ٖ"|U�h����l�<.R�����]��~�9SI�e	��F���"�tC��a�LF*� ʋ�2�aò,�s��ܠ_�V+���c��$l�[�=�T�@�� �g¤i�` ]��+�����:w�d���/}�8_���Q��4ԥj��/?h�T��{��i,���s-��.h�4b����^j/.Q[o<�Ϊ�k,�I�PDZx�k�ȹG
�%�9)�|?K��b���5�&V���N��G7,�q!�6)�d�J|
�i�u�B+-�qP��U�� �n�J�sb3�qh%ś�ܳ/O��y�HG[�濶�7P
Y}����(g�Qb<�\�8�}����Ë�����$�fd�A�]\���#pU�q(�zĈ���2散�D��c�tx��ā`7�o�7�k�0�r�|�s2��J�����$�e	�(�I����0i��d0��FC;�˾�C��v�����q�m�L̐`a���/ґa����kv����TR��	�95b��S�w�;86;y��&4Y���+�ce]K\tu�$���,����e.�x�G3�,6��Zr��Q��Y��IѕA�G����2�Ա7h����RޘNk�0iY����N�{J��]6:ѳ��1��/�ݣ%�W'�>�lg|2�ч���n.����X�ZMOp��FO�S��*e�]2�)>�2��Nr�krw}-��&(�]!��m�޵���[���?��ssIF��4Pu�Qu�߅����<�L�xw;5@P�&��Q�KY�hWC�z5*�Q}�d_���ߺ[�_�gEl�φ�s��gh�K��@�̀��Gt��/7
endstream
endobj
129 0 obj <<
/Length1 1372
/Length2 6219
/Length3 0
/Length 7147      
/Filter /FlateDecode
>>
stream
xڍtT�]�6!�!!5HH3t� �9 J�0�C��C7HHJHw����!!
��|�S����Z߷f�{�s�kﳯs�}�1�xe-��%8�+��h�d5�  P��gcӇ"�!��lO g(&�y�����_D8��b�J
�I� A P�/"!	P �B-�| 58��&w�@@�m����_� 		1��� Yj�4�H���`{ n� =�U�C��t���wss�;8���Ҝ< 7(��q� \!��_�Z`ȟ���� �6P�? ���� n{��|����  ��@� mG���������ܟٿ
Aa���pG0�
�XA�! m%>�;� �Y�"����`W0�l~K��:�$� �*�S���t�s���������1+�,����W
P����=���\;�����

���%��ő� ur��*�ɹ�����!H�PH@B\  q@�-l�m������j��r�;�ne@|�V��?|/g�+�D�@|��3���� �j��C��0�����?ַ��������  ��~3�u�%f�������?3Rq�)���%������ @��� ���� ���C�l�?
�¬� �?4��_:\�4ǟS�	��Z�[;C ���(��}��g�w����������\���9� �?q�������=��C�S�	���8�wT	�Y�����uV��C,u�H�?���܎Ŀsap�_�#("
 #`|�AED ^�Cf	q��M ?��M8� } Vp��+�[�@,�~��aa �-z�-��\�P��o�_=X� �����o�k�{�!w�����a�me@�Y�,���Уq�M�7��^���b�β�WˈSلޏ��'�瘮���q��t�.���b�F7��gF�>�ۓ��b������}�������M�-��E�X'��̭Sٽ��p~ hjSw�LT��p�7� ��?�-���$�}l$/.�Ww����q��&�n|���\/����I��b}A�vZVZ#��Qv/��D5�i��܅O3��ζ̈́����dג��ɽI��a��#��C��5�횑��D҈Aet碃�3-�!K��ŋ���r�I\Q����AK�<����O�-�DŵH���X��dF~'gFΏC�8>���(i��Nw	��}����r,#V�9�6��;�����dC�^u ��a}�z�it8x���=�Em�w��%8_�	��U�����Le�%f�����Y)�(�M�J�ነNў$���c.��stI��)]�:�oPn�&��1���b?��p��ZJ����٢}�0���[���4R�_�z�����vї�k��l�]����{�Xx����_����_Ѫ#���]�0)�y�b`�*���~��n���q^�zI�n�.�L4��'��f�3�;�\i?�@(="M�VRN,z{��Ĭ�S^5�����a�ޔ#�������-�p�Ѱ�B�$W�Y-+*,��QΧ�K��"Ӽ�=#����q�v�E�?��iEm�N4ڡ�X��L���1�=�nTB�yE3����Ib�?sޣZ�H�#�/��"���5² dA�k%�-�7�4d�$�1Js��bL��R�(*�kX*���@��~��D������b�:�āux_:36�6�%UABG#K�6�v=+��$��n%�������a��CD2�i���|�f�>����Q�1�I�\a�~��Z�~���y��F$�+JA�1�����mҌ�n�����l�j�缰��������gU�y~�S�j#J�ɖ��+��{���{��]����{Q<[?@[���F�Y��ڢ��F�E�
�|�:���z��*�kt�b�ȗٺ�+���z+���H\8�y��6$O���]Ax|�ƺ���8�Y\�vr&�P��q�Y�C����6ҙ�fC܏�k
(4�d/O#ˣ��Y��BD��/��ᔩ*��FT�e��`�q�Đ�6����j
x&X�?Ƙ��~`K`q)�)Z�ຕ��D����}<�R� �}�H�x�'!��(5Ms'������8�@�D/2Ey���C-��֣�r��u�8F�nC�� e��+� 7� kj�&�j�uLu�F(�$���sb|�7kR���\N̙α;��(�Uv�$�e1��!(@$��w� ݎ���AU�媰��A�\t�X���:eA-�(c|?V��4�Y�	��S��JNHFdᬏ��'���������17ι�BHԂ���<9nMx�e���7^:$O����ԴM���A�]�3��^�=77K��xa��n�^�5Q�U5�m�N%�=�r��g�����ˊ3�#ѹo3E��F�߮���۝��We>�r�S�I=[m�:ʂ%'�e	��(��~���VE�9�X�u��g���BX�����	�����+�|d�l<"����jp��U�L������
6n��� ������h��1S�ob�cL}ɡPo8��5p��gƏ���R���;�~�z;�$�/W���mp��ıC��;뛷���<�ړ�RJ�J�E��h�TRG��lŕ�q}�t�¸Ŵ$c�/-q�#j������u�!})LL�:/��\�s`�%�+}
�lˠ�g�6�w�⩤0��������H��L�{�ԧ/[1*?�Ӈ)�W{�jA���'Q-���j���4^F�`SǺ��;oh>��_zv��ml�A�帉�k��V��Tf�sO-ݝ(�0B/�ASl��&]j��{�[8��P��of��lry�b��01�i1���񽾬Dm�8Y[f�a�ײ&օ��*�Q�]�<�{$��9o5`{h���U�r� 8����D�N���f���	�#!
�D.���+�7y�!�N�Hښ�����42,���}��Ak͐p�����y�]:3^�v�WzJ����3�O9�t�~��j*���qJ�9��c_�6�i���w�Gq��8��ZSI ���Oc{y����/u���w+�l��=[�b:�Nn�H�9p˪K���<6��G1����9���i骯��ҽ5�<C�����)
�m��?�	0�%��f@�eH��b���<؃S��0���p��7��+e`-�a��ޖl=��΋�[��\|�sx^��$�*S���t�9b|@�F ��&�\z���j��T���oT>�D�1o����e���Xu�������X[ƣ�;��,}u��:Zӝ'r�C��T{�t��iWw&�r:���=��a
��^1����NPL�m�{�O���n�VF��Z�$7�P!k]O�*�@�.1u�Et���FڕK!|�/�<�:�_U�*�ݣ�SaS,X�X�9�VX)�XGK��u�v�zu�P�G�<�!��� �K��
rAV��:��8-���ǒ������Az@�1ﮎ��w\�.�$+Iz��~�|�T~��7��Z�O2�������_p�¶jè�v���{J���&A-C��3�*���T���)���]���ꘙ�Ď适0"��j;Z#v��5�� ƄM��d�(J�@2GL)h���d˽����ᵄ(��)�h:�Ak�'	8d���Ji얾T�$��I�=��$��)�JJf�n��K����̼$S?f�ǒ���?����d0�ͦ���@��B6�,�EH�@��8��h.�}���,�P'"�xg�z�ᐻ�WC�#��Ļ=�L��3�ʐ����9�).#����sU;k�jC��:W�>zM�����y�Cê�@�K��+��`MEÓ(�dh�8aN�>lQ����%�����MMr⊞�n�J(3f�,�j�:��_2�O��I�D�l]�Y{�׼�� P��y<]N�?+^Z�zVM��ַ�?,I�wݒ�+�L���J[\YO���Ȅ��;�rq��P��ę;ى�B7$(ə�D�@^��L�Z��ױ��꧌����m!���*�������;�
!Ã������׶��Ē�{���HҊ��Zb�!ہE?��t��	�^^E J�y��r^~�1��|�0��w�#.�Ud;XG:YRI=�L�F�NP�B�K���N��&����)AM�����]��SV� �gd������$M�ˮ^���ΰ�Y�w"&�Z��5QDN��[i��d��[;q��	� ��K���zT��l��r�6L�D8��dX���"J�x~@y4*�P�awr.i�f�}W3��a(�k����Zu%tЮ.��z���݅[�b��dk��>y��H5m[dxD����a�6ɕ�8'�Z�2a����`w��c��g����o��yU�1ێ��L��]�o��=����>�^\ ��<��:��@�t�=�1�pC�O��B���؋ұ�	��u�e?8cp��١RENo�u��;�Cb�{W<y�Y�=�k=-�7�RT+%m=u
�QN�;U,�ںK�GxA6��P�,%�ɰɺy�PlF������Aw�pj]�A�OD6.iY�P��-0N���]�)��v�F'���*+ػ�ѱ�c��x��Y�)�x�S��o����b��YÆbw5���F�"��ޜ�EG��p�U���}ؽl��Ü����4.	33����"�񆄟�7E�vm��U��	�'>gcK�~R���ѬTMw7�s<��5� &#�������F�{��k��V�}�H'c30�d����̷oMg�v�����9��&b�t��r1�J�Ew����x6���Ph�#.U%D&�ڮ���7.Rh	tc�>�l���_F�cO6,=���ܽ��;/f��LnUU�j�PЁ��O�ϫv8�z��-_a�n�&��1�Έ�A�xO��|G҃H�/e^Ų���>r~-U�|$�^���̋�ԯ��`���r>L����F:~�U��L���yzG���<���E�g"���V8�d?&�oϜ�v�īs����外6�)��a۱o���$��� ��t1�M�ᘏS��M�R���v��U4�ɫ�-p�Q ��:Y>w��T�ۍ޽0��3�T�[��N�}��>�Ӱ�|�f5V?ɀ��}�o�����'^}/����:�px�|N �.�ʟv�Hx�:8@\T���pp/b;�}$-p�i�w��Y!��9��WSfy*���d���;���2\g~��j��;���Ivltp9��}M咢�x@Y�/��f7�_H+���q��}P�|���e�9��R�\�P�Шķ�r潉F�ytK}$�}�y)&K�]�k1��ғ��u�/C��䊻HW��x�R�~e��x��l&�j[�a������D�"��#�A�g�ZMK��7i�a���l҃.{��UԆ���k��D���c��!���Îx˞�n�1����6Qm��&���hX��nɭ�������)�f�8���b�1�)�]��G̡ym��4�6�����\�M�[�������nk�>k����'��;Rpd&Ob���>��[�-�(��Ύ�a��vN��w[�]|��FQ�f1�dz9/�vǱKߕ�D��W[9�\`�8�z�n���Ł����$������-�;��Q�n��<��{X��9:�������+�ɧ��d������)�=��[;��B�,�4�����D�?�������)`rmʩcS"緻�Sj��ӣ]��P����H��O��;c�d:z�7��b{e�A�.����N��6��O���KP���Gps���ߦ�X�!	�L~��b�c�"�ƞ?P7�~�u��GH\5����}������\��*TT�Ԯ�r�˰�\��G�^4x�x�co����<��u�९���5�	}�>�C^$/�Amx`�t5��Q�ļ�V�U�Ԑ����f�#*� ���{����k�$���vn���)�Y����F�lN�\�N4�&l���Q����"���ӻ�l��وS���T�Hէ��5�|�t;毟�){;��W3�Nf澇=!���9c_�mu��/P<5X�gh�۫�Z�[��s��沮�f�0ţmЙD��D�H<c'R���(���p�{�?�3�n�2�X�ok�]�7��G��+Ļ�{�z?\3�&L:�*ؕ�xPr����
��>�D�O��5��Hn��Ps�6�S'U�QOOhna��+���0�5t��V�]!��l��t6��ac�t��	2�h^l����i1�EiӔ:2�ʪ�:�5*�zG�V���ߥ��E������I_�h�{G�k����ߟk�fB@�k�d/��Y$]��,4m��D�*󐬁�`M��b����wfW�}����;.����U�b�1��j�
4��l.��!A����C4(�VYS
�3uZzn��1��jN�Zlĝ�<��j����0�H����|����W���V�<$�U�_\I�jn��]tSA���ݣ�♃݂Pՙ��
>�eÚ���.�v�"ex̾��1O��O�����kf?@����2»�D6�ihi��C|��6��s��M���#{:���<*���Q|��:���-S͏@Ǯ=�WΘu
BY����(��PfAU��TG�����m117O���t�c����4
�uZZu/5$�`�ϑ_�籩TJ">B6,��ȼ���@D3GK��y�b'���M2f���j]+��Z?�e&�E�9{ݎ��^�_�3{rE��R �m�3��o�j��۽��oLe�O��w ��r������Sf/�dȑ�N#ĢjnB��L��0�ؒҫhR�qeю�Deq��b��IX�hT��������e���T�������);,o���P��耋%'��5���¥��Hg�D��. �jdT��,(��T�mA����e����3t����o�;Q��Z�i�2��P�2��;�V�@�#��$t[O�����9�	 ���x��̥?gD�@J�O1&�m|�	�XiS�pM����\��G������2)��Q����W7�K�PӷtS�JT3CJ������4� ���Z��V���G����߀"����41��~�N��t6��^ra����Q�� �S�
endstream
endobj
131 0 obj <<
/Length1 1324
/Length2 5942
/Length3 0
/Length 6846      
/Filter /FlateDecode
>>
stream
xڍwT�k�6%R2tw�tIJK3303t7"ҍA	H#-"�-�4��tJJ|��=�;�����k֚y��������5��F�J`�=DG�� t��t%BB�BB"����P4�L�n
A�����"� !@��
D��!� m@X ,!#,)#$�����=�`�� @���Un>H��Z��#�������Pr� �  �D;A\oNa #
A��+���&#(���% tE	 ��
�| /(�	��� =!`�/� =�+�2Bv���7B8���H��AA8�&�� 7���t �n����� ���N�W��DP��` �pu�}�pG������|  ����7�@O ��!��9��d ��K
����Q((�D�_in��� \]!p4����T�H��>�:�Gx���2�p��/`7A8������"�s���B�"B�" �; �r�������)��Q��p8܈�@ 7?�~(�'�Fz@�������0 ��G(���70��}�|$�`)t3{� �_����o����|������Ceu�?���)+#�~"� ~qq����@XXH��4�?��@�_��'������x�����Xp��2܀�����e��ѷ�|	�/�����������������������P������o����B�P���j��7K�w��]<(J�@� �?���ܬÿc�� ��ވ�K �H$Ї�k7�8�O�f���ߓ	�#�7! 7t ��$��))� �	r��+5���Y�߽�9�?����@�! ©	H6ܹ:��R�ދeH~�}�,���o
��qN���]���<VJ��@6�M��Hq���o��/�)ɰ��������+̈́�#�]���J5���Ɗ������!.؍�m����=�H^R�z}���,���X1\��x@tQ���I�UH�g��9�,�h~F|�=�۟���(򆯙���l=-�{�U$�l�w��X�����-#���(���Z�6���ٮI��s�T��T�盵���4O�'1��G�q�$�KX��F�0I������S=�!����W�R��x�D�t˱%S�/���b�B�q/CLc�ţ<s4b���7�jz����ƙ��89�G�i��_�qކ�`��&Z�=N�U���a����f�Ҕ��`����׮c�f�����6���&�.J�d�&��Bw�%��b�[�<S�&�˅�l�{��^�yi~��X�O^J�G��mY�Ӑ,���A:�/'Op>|Vf9�v\V�����/]a�[A�����M�Ƕ�O�W���?�Fh�?�`q�u��[5q��i� \Q��b~�gm4Z��x��rأ�fo��;���(��~�yҋ�	�0�1bӷ1a�{���;V0�ErB��/�<�Sa�����<�fSWM����~*Vn"��Z"��_��^�8kG=��s��axj#�τ�|����ޣr��:�H��Wm���n��<\ֺ6w|������c>��M�Wo�&G��'SF�\r�;؈���G�*I���/��ǆ<�R�8+w�0�}�b��sy/^�,�9��&��bp;!1�璈P8��v�됻z�_��f�,�!�ru����\b߫�R5DC����G����������(��so0�=���o�#��e�T���f�;>h��0�Z��HsLq�����ݢWf�j��ԇQ�!,�b�uK��.��E��uj0�wB�ޣ؃QO��z�H�1��8���f�A�fV��jD��*��I;u�`ZO������Qs�Q��R�U\��t.\H��I�������@�k�b&(���hV5��2�@0�u�Oun�F��Wa�f}lc�u_[�|���3�,�������
�o~�tp�r�ah��/�j��7���X�m��xb2�w.��wʹz��$G�s��pi�bK�b��G戲2�jO/�z�X����D�~��ƃ)�3�M�")���$�̻�3샩֔W�vC�ቫ���]�H����H�U�kp���Gs�'�|�e�y&��Q�y'��t���t�xj�~�R��#-I���"�Q���.�G��)T.j�n2掟�5������_����k��	V}�o\�C�?�:� �G-4$��"���>�<�Ȧ�k8�Je�������3��%m���NY����/"喳i��ZD8��5o���p��6nk<�>��wy.6��O6���fذh�wmm�s7���p���-u}^�@;�j���s��!7I�Dk�
����W3�bb�K����ɐFj�-�nA����RNzms�1\Ѥ^�
���p�]�,S�烘�>{������d����z~�·�!,�{�']�n���q����m�O9H�׷m_�֙������qcgk��۪fz����0�ZaI_&>��Ƚ���M�%��|�Z!�#��:�����������~����ݾyw\��S�)R{w{ Mu*�b2j\I�����I��9�TE��=%%q���'"�(/�! Q�e]�����I(���|ɳG#C��3�H��&�="]W��&O��%��?FO	ә%����A�P���<@lg�GO�#Jo�|U�_�;%b��p-��ؓsT����~�By�;|a�ˠี�V��a&2v��'�f��������aH�[���ƈ�io�nǰy�pN/��������M���$����A�@��.�g#������F
|�����M�@��3"qv֤�iMf}�c��\��Eq����t̑R��τ�وໍ�������j�g����n�����J2�*�NEP�����|3�'�����i�mvv�s8��5�F	��T1%1�0^Gi[�����
����Ѷ�u�� �o�p��ȩ�t�hl�o�jM��W��Ҩ�l,jA��%ǌ|����w�d�Q4�����U3P��P�S{�7d�4Ţ�c��qx=�+sֱyѠH���qJ�Z���s�h�����J��(�C��1�i�a�����RJl��r2k�S����1�5�ˈ�P�<���8w0c��v����{��~�F�e���t�%/u�����:@o��|��.;���wC=�����M2;�@�X�.5��!�uҤ���̌�JK�,o'�\�.�,v¬��W�=����C�O���ٴ��t�L���c�y]V�{׼��S#��j]�ƾ����S��q��Z+sI<^�E�cݨ�D�&�k9K<��n� ��j�g@�I�T���Bai;�8bWA37+w5NT�K��-���:C�Ur$�����z����u�f�̂��{�P�}���-8*B�Ǜ�8�>�5s/�\Y���3�����G��	�M�Xk�|'�?��R��x|�$w��Z�<��,��ͻO�/ۺ�xx�l.�,�򁘔���?|؀K	�8��T�����~fb������\�S	H�1�vF�kRHw�{~��!X9��
y1D�m�	�w���j_j2lQ��|ޒ[�H�ѱz�Z������n�>j��fҝ�z�{�E����)q���J2�)<��p�%ȧ��aQw�����8_P��K����/����66����b�/���9�Ħ3���(�_Q�7K�{С�zB=�#%A��z���q�y�.�Sf��+���kt�V�)�.eP����ae������ޡ}~NӈZLI�(l<_���;z�Q����W�Zdc��E�t���7> ='<3v�1ߤ��@���Wq��tx�'��̉�4e�F)"���bG����3/]?1�e%*�ghJ��u��2�S��򚬜�:):27E�_&�����A�=jD#���;���_*,@�"q-��/|:�����$�恵	�/���]嶏�;2m��	%��y�W�~��󄸚��-�a�a;Au��l�էt���$�{~�݁��lېl�\�'���+�v�e����>G������Z;=܅2�ഉ:7�����a9n�gm�,�ǥS�M�3U�,���>O"�������=R��в���zp�i�믥t��4cw]�9Qg������[q�E��Q��Û�5�kX���\Ǉ��C���ݘ�hP�o	�F뺧���3��`�L��0a��z���D��V+Z^�'�[�*R�A9T*��q�t��	�d:M >-��`���ȦZo�{�lӚN�F�}�g`�C�ԻY��}#_�lA[������L#u��{۝薡��d� �D����6C��[8
B����������f����>���Λ��{����O�Km.�>�����J7)�󶝸��P�r9S.y��J�B|��u����;��i��h:�b�҈ݵ8y����������ePU�9�%oO-/�눙=}�e�D��h�=����ӚA�d�gk������P� �
%.��+�_�ս|l����V��m1��y�a �J���[
gL<h�nBFQ����Q5�YrGh�_m�`�[�c���b+v���t�=o�����y����X��������q��WU���Y��1�/,qR�lN�����s�[��~�`C<��_u�fF�C�b����z�h&s79�@g�J�p�WW>ym�$=`��i�5��-�4$K�&���*F�(��fQ2�A��ep��gd�ˮ���7J}G
ΫސB6�C����F�GRe �w6u=�Yu��b�J.Au�Ԗam�Jc=0��b��naQ�Z�|	W� ?����^�0�WL�T�j�W�>#T�a�U�]�3�r�soWX�|J��٦�E�x��-�xFb��>z��oTud�9��D��7�UW?�q�~�&�n~�gM�;�a������F�@���~^��(�d��z���r-P�h6�m�I��ajCuj[�{���
wo���^`V�n]1k����DY��Wې?�g�!x:�S�K��5�`H\���
�H��h�[���!\��Iҋ��{m}�����M�<�l�rG�E�msf�k�w��/b1�I�B��YY�q�fi"/)Q&]�1�b��u�Gz� ,�؀*���{o���
�,�c`+��-M�yrŔ^�kM*�3��<-祢�
��U�]٩��W�@EL�VչwgU�"s(tڡ.5�78+�7�h��y.���v~��Y�G��,�����s/�=�,�U��{����Qy��c��v�xOy��y�y���n�|�b�"�P��ƫ�^��E�/Ry�H��Py_)��<좧�M��Y�z�
�W����ګ�*��_��K*/� �Ç٨�����N�5�3ϯR-e�d~���������?e�=+V�)_/���k��TB�#�I'��麬�֡�t�XH��,+����4(==P���s���7��L���@����ٍ�}i�z�v�s��B?F��� {�`�� b���>�p�����R�Ƀ~�դƟʯ��xY4s�?o5�}�ź���6�@�A	I��v�X��#VcYY���}�Q2��_5���
=��㔉���V��͑}�U��$t�E���R�j�\{�R�QD�ks�v���[������p3���`�h���v0���͠ � ���C�7����Z��a�[zw;Z^�<���P�������3<!;�u�8�h��l^b�y���yu�:�I������b�n8ѐ�Ru�z]�l�I�v�u�}MSz]��ze�,byp��s����aEDK�cy�\��W4m��oȀh�	"'�Au��hG����z9|����Pt,��R��w%����W��f���)���e<�
F�@�F̃u��uW9��7'?��K�h��f^n��er-�V�࿍�tK�4s}/��_�b,Hk�ɚ���%���ܲ�ȼ �~fk	�~d��]�m��{�)�l�?�7�j��ǇF`5m���@�b�0��!�W���:b��O�
�z��L����d�A�V��	�������x�w��Qj�l�f�и�[8%9���K�`iV����G���2���^O��Z�t�ʼCe߻<�L�o���}��F�$=6�&�d�`��5%�������_e�4���k
6��m샱�Ii���T�����?�mxG���#�.V���K�1��_ۑb�'=�J�ڶ�
Ty���'��iQ�"v8H��p��I��ˊp 	�/N�h���� kf�����W:�ݡ$��*��d W���qU��?�V��"�=�R�.��ؐ���6�VJ�
��ޟ ��N��9R������C'�A�Ww�J�v�o�P����cs1G^u����bUa�mv9>�y�L�0sު����l�Ʊ��/�@J��LJ�D��|ò��*�]��X�WVM,^ٔ��_k�neŤ6����3$R8ŉ|�-�Ž�؅י2�B����k�TG0�)�����e��9�J=M�)r>֋���Q��QB��&j��F]�c|9G%A�'DW>�x�w}��,[c=�%�{�� se�=��<ޑ�.��y�;�W1�����E������[��D��:DU��`s�v����qCS�[�
݆G ���f���Iu]��s���WBڤ�4c$�'�:��4�6i��Su>�pɐ��Xl�=�h��,T��!d��o��a�As�_�r*�npd 4��	�X�Ez!X�� 7��y��(Z�D���k�]{{T�
�j�X�3+�d�|�E� �S70
endstream
endobj
133 0 obj <<
/Length1 1626
/Length2 13193
/Length3 0
/Length 14033     
/Filter /FlateDecode
>>
stream
xڭyUT�ݖ-���k��!�w��)��݃������-����>�ǹ}_��C���%s͵����(H�����mAN,�̼ y#gG%[y[Y9����΁@A!� 4t���:y�@�(���
`���A� ��ڹ;X��;�U��i����e�+`�����LG3����hmkg9}@����@��9`ja�|SД�� PKȫ$� ���5@����� ka9i �� � ƶ ��Zsd����08��->Ҁn�@��\� ;�������7��`�`r����-�dl�l�����߄�l?"l>|`
��N��vN���
�����dn��WmG�7���#���������}�|x�-@� '���_��� G;kC���`v�pv� ���=�hf�`btt�����k:���_�7���v�;���r�prZ�2"��~�4v��mfB`�kW�@�� ��M�����t�{@���	C[��;�h��$o��Q@�?S��O����W�_���O���\������������6��7���� �@��C�l���XX�����=Z��¶�&��r2��W�ه,̌��0Z8�[�M,��������ۮ
2:X[����=R 3��T�-��@	��d���?���<����Wu���q�;P�c	�T��>��G+r�&�<�#,l��d`��0���|ܽB<����Mɿ�X�u�3tr�ph����w�����I��`�@ƶ&����!��c��i��m����!�ߗ����<���@��ay�֘/�25=ͩ;{pLT���r0خ�N%��_�m�oj�O��KU0c��[������4��P7�5UW2�"������'�:e�~ �^ѧ�S�(��9�M(-Nf���1E%��66���4~d.?�0��}�Sjc1�Q��Ы�NN)�~?P��躆��çˊ���3��I:!���n�pWg����e�:�l�%�,�o���Cδb��7����B�q{�����F.I-@7J��PTѹ�h�F������.�I	��j\���'����).f�jhi���f�p�����`�P�2n�q��I���5F>�/�F?�#�w=���!<�P�?<����P�`V
4J�ᮉ�����qƩ|CS�xesxV<��6��~�B�����'�k�Tl�b<��ld�2TN�X»#`OX�,`��o$kܙ�3���/�2��'�+���;'~G����3?�	��~R���1[� hsá'���4�L�4�`��ce�o[P��JI�9OI=&�c�����uz�c�f����s�����,��A0�-�	��~jf���aF�فc;�P����]}��������p�sF��	1�x:�%!OI6�sx���T��i]I����@�8��qQ<�h�25)}�{դ{�����UFNq��'m�H𽠮1��9������V�]|�WF\զ)����V�h�������Hs�`�7Ӯ!&�����wu������x�GZ)m�ٍ+��g�HV`��P]�\y�sz��0[�PⳖ���h�ϋ�\,���V;�^�#j�V^a:v̲+��$��q�g{ S��ۂO��ghJ�k��j?�B.$�����t�am�x�3�N�몥0C���;IO��"�v�.�]�AwF�߇-(��4�U�IL��D�񒙙5�a���׵�A�B.���vX{���ٝ�2�,�M_E����Y9�(�=j&�9�--r�F��|ʈ,6i�����kS�#��-¸�n�]��|u��v�Lc���ХT������հ��~��M�΃w��>��WD{')�l�~����xs3�����jE�I3c[q�?�u貱�^�_|��=G���Ys��m���j�0��<0���;�ǦK�h|+p��b��OV@+��F��ӅWW0eX�Dq�5�ڽ����:�^)���0��/0���>�Z4�W��B��[�0,�6v�W�֙��^�A5��^���^p��^�/��Q�3��A�]����%�����P�w��38��U�	�����c�5�C���7(�Q%�=S�@����?�������>���OJ8�kg[�
� ����#�`,�ٗ�`�`��Eq��8����~��w�lԠ$y�XXi��gx58ҥ��m�&���"4T/��b��\|�V<�<L-��aF2��F�q*
�����~RtQ�T��*�+�{l�q�D�,+����/�m>����`�++֎��}����T�.�D1NK��O����DD�4J�� q�Z���W�7 �26�<�R8�kh��ڗ����_�&�e�6'�k�aA������1T�j����.�����?��*��Zh���m}���������,������%L���L~o�p�R����@ۑ枦�d�GǤކ�=��:���N�60j��.���*02�C�\�����3����L�J����-*H�k��Ƨ��!K��ὅ"�Ŷ�K���k�Z���zӔ�?o�s���B��,=�sZשORPW�S�)o�j��SE���wJ���~���q($NK5�ܫ H_L[�yҧ��}�	�����0����VR?0J�a�_��wD�ɔ��N�L�.o�����>⫴Ks��A��hs�h%��Җ�ƫ��F���%�al�s�	��M/���,7Gw�c�RN���m��vI�LX�z�K����Z���*,c����{Y��:���d�A�"�s�l�U��Eg��{�8�_�q6a)*$4�z�u�fO�1�Z�)�Ok��/6�>g��0��7e�h$�W�����L��H�^T�'L�^����`<�Æt�O�P���fL��|;X�s��.�_x�[�/�n�	��d�roNl�ien ��:�׉��R������g�B)��tE%*�K��h��j�U�=G�ȋ� fW��+�
�)I!YHcI-��5��g:�����6��M��.Ul��JE�ԼB� 7����P0��pɘ2��n2���\����b�P̱j"E�Tb��:�&��K�D�����en��N&�?g�#��ٔ׽-I� ��E=�C�`��ځ�-s}X���ud�ǂY^�Ru�?J����i]u	ype��^����QNY��}�I����ucn��t��7��,��W���}o���-�
_�O��3fֹ���=�GXoQ�gƶ�LZ#|p#a����S���7f���O�P�O�*r��D���� ��Nc,�VL�i8C5��޺X�s�p�yU����0�����@6�-[��t�ƭ���Y�o�N+���vn�:;��/���O�����Y\c�l�kI�H�<ߓ���Ԥ��ER��F\P�L^���A
�}���ţa���L1�纩#-�vH���2y�8{2x0���<�-�����G> ���m�s3�w%�8S��&l��
V�.�K�A;x�k$
s�Dp���L�2�76-���Xb�?)�r���^%�,b�冧ڿ�2�	�%�2�{ =Y�ӣs��ڡt6�%pj*6�/лRS�(�}�|��3�]$$�BzН%���M.�E	�qB�B�5^0	�Q�5���tFA�_�|�*F/�����0Y�lZ��F��7.Fx�00Mߢ+ЋD�o] ���+�
�˗�Inh������4�l��q�j�!�A��]D#���p�?/��=��D���dmU�6��U�Y�:X� (��ʉ7"�!A'�N��ͤ���#��� �ڸ9�+T�U\��0�g��Ĵ�Z,e�	�i]�wcG)석�P5՛��?he���z�",�}��{0��S�Vkٝ��>�����|2�|5�x��u�q�%Ե�fܕ�e��86.�D�f���h�J�l�7,�i����H3� �bpR�(,n<�`yEֿ�[*p�k����>D��'�0�O��y�QZW�]ᦱy�:Bt�';�p��A�;Jؒ���-E�!�_7�h!�3�U�7�.�zi�z��*���J<��������&P��,�&e��KP��}�ԣ���$΅�ߛ�<Sbվ�2A~�"��.���DN1��:?�Xf���e�E�FJU�@�}|�Q��
�w �}])��&;��9�{�~a�E�e��u�*FQ����
�ڴ��l������6��uy11@P<}SubIJ�Sq7�t�[=�L|T�d �WBOSځ3�1�6�yI����$�Ʋ��e���,g�	L2Yb��jk��oʘ�J�j��ʟ�$�^^��|{�"�0���%KE�z}��7<7B&��-��rH�DHމ���^�\��čݓ�F�� �}�b�6�SI���nh���;�*3�G��2W-���F�"�a|RV���]bv�� I�h�e��2� ��2CP�&�^��z��|����-�ޡ�kpݜ6�6,L�Wni��_<��H'E.��I�iB���aO���!��C�v��z�� 㐺SFs�����GLRf;��i���ʎ�tdWx�$��ǉ�/�n�&�}Њ���,.��?�mC����pK��y�I����]�#��=K��!o�<��9D�֨~l�|��ݒU�� ��>��7f�,Oi~u\��R|��vCyH�c����	qM���'�$ t��	l`��vox;X����C��&K+�����k���b���D�Q�𲬭�f933m��h𹍓^�}���6�ą?��i���;e��W���9y#��Q��Iq�#���$���V�o��i4[������
�$@؆6*�G�OO���#kÎb������$@� &Q.�A�����y�π�y����<�
\�O�(TW���v�iij1���r����l�*�b<�3w�؃�#u����6����J_5��*<c,��X}b��̥O�����X��0�*4.�M���J�$$�^4q.x��;����S�Ύ���T�/1c�ݻ��0��<�A�ņH�!
Ǎ'M	dK=)]2
ET���~"N٨o�R2��^�	[)�Q$��"w\S�-�-�>9��T8yoA4"`��I�7T�C�����͈�}�ƹ����8r2ޏ�wC��o{�ѥX)y7X�ـ�1Ѭ��,y�T�g)Oh����sO˅���������_U.���_K�n,1��о'��������ҮkC��qߛ��7C���f��S8������ɺ�<x������&�XjY����R�>L�� �9摥ղn|��O����dxis�9��X���+��m��p�Us�!������Zޒõr��� �������-�A�l��Ϸ�R1d4�O��(����v��nړ�[4��c������X\��$���j,���*T+�師f����>C������yG�&����� ����s`� �c!?��� ���c�݆)�W���0o
��ܾAf�ǩ�5������QP �Q]�J�����^�m�>���M�V%���|��:��U�f>��{ss���C�������������~]�]�����!X�- �p���#���_�"Q����/"2��2W��~�gR�U&��1,УĹ^���R2|�7Gh[�i�_r1�i5d�����.F{n{��ð|��ߦY�N�������6��0�H.���D��$�u90j9Ʈߥ��f���A㈞����p`b��]T�g��ː"3�`�Qj�T�K*�t�x�
�$
ȥ*ͦ����P9?����37/F�-�ܙ�9;��D\0�Y����c�5z4��a.�௒4yX�W�]{�cu�v����Y�|ͳ>��Ӄ��p�zd�Y��k�KM��,l ��S�C���� ��K��׷ӠO��Il�!R��$7�X�HZ�
�̹S�PD �p�/�	t��z��k� V2q���]���r��7��Bh�#���t?`(�#fƀ��x�<Ԕ�g�a�������������"��oĂ���=���}�ܢa�~�v�6�Qӝx��	��M�P�^���)���ע�C؎���L�Gt�zE!�>����BT�@2V�u���@�s5���,��a���h��!&�U�B ]1sx$qqY�Z��텂uD
��eSHѼ�DJɕ&���T����:e���Tj�����D/Fߙo$��n�\#�R|���['�C ��(�{|� �ȴ3�Գ�;�z�.���L��6��4{�\�:�N�hrï�d��m!3z1g���HD�0��䬊	_K�M4 ����R5�<�{��]����Zh	������?;A������d���o�#fm���Bo�D����;F��0���ֵO�p����Oˎ6���U�H/�������}�.�tyRR&�����)]�L�S�뮴��w�+��]G��a���l�ce&�@{ ��bM�j�����|���$��&��2Q��[��IO.>a��D�޷C�g�v4���Qy�5�OT�3ܞu*� �Q;~��._Π�c�E��-����� ��:�d�bN\���\zWxj�^�`�ث?�\����(|� q�3%��W�r��7n���=R�V!p9��r��J���I��2�5�]=�;�]���2//�J���Gń߄1ஐ/3r�GH��o:p���tSs��OfV�Y��:���z��U�"P,_϶m�}�R��?�.^y�
4�%現��J�@�Cpf���[��?ƙdɲK��O~g6`;NЊ,Q [���Q�b�r�0�0L���:�Q�?�C����q�ӯ8H{�&Rh��7,\�|����M�KW������֯%}��Q,D��}��:���r�����;V>���Q��t=�jq��qǟHdN�_�{�Y��ϓ2�Mo	Ʋb�r�����������������wVUK)�A�'3�JJ��%n0���k����;��*'e\$(Ō����Z�O�1�1�0�i���Ǹ����B��e��9�ʣ.>�㮁%5l�מ@�=*�V��4��Qey�dK��?E�|(��/�Eض�3�/bB;��~�G�_�V��:"�@V��N��-��=(W�MS�EIIC��ldx%ٙ�x|���������W6(ߋc'���'�����.�}�7�פq�*�0��
�vb��l� \w� af���
��}i�r���}���^a���h�a���o�$;D6$��J��P�:�ʋ@�'���f7�sU�Ǚ�*�����s��|#��-C����Mv3���5	�'i�h)m�ި���yi��@�&x�՚��}�F�J0_R],;��"��q;JBz��o26��\���ʣi]�f2�F�f��6���{�O�d #Η�P甯|�r�f���;O!��B��fq޵��
�\�ɆB�3�_��nSS��B`��&8 ��!}_� )��i�!�0cL�saEko���9�1�<��u��(�g�U 9!L	K�M �L�@i#���̽?V�/g��p��?�i=���?���bnl���Nۋ�gm�~m��SU�&�7�V�$���s ��L9�o�=�S^��R��n��U�8�����o=+�Kg=��_Q�B(���VޑQ`�Z���JO�L��(T�������8w:J�*a���m��Lϓ������6�q�c���-�C�&'Z�+oc~�R/uw�a�7���N�>5�������_����}�JŒ���/�H��ot�Eڙ�bp����q	�ac<̐�/�%��K�����?�o��im⁯�,��2����.�����̹f�F�E�s�p�sM�P���ok�|�b�X��F��J]`Zo�LV�;ӎ�c�uM�:��p�zQ���Մ�����5��S��'8m1|?�>ɀ��!��� �R����Ge��"i�3�a���1:y���R��eh�=L�C��h��U1#	.������� ���g���1���Bvk{���VoE
��{t�8A{�?�����oI��5\��H:>b8�����쎺��v^��W~\�6��_�H\P�,�9��3�y9kl�XlzZו��'I=Fڪ9��:IM�(nP�l��+TLxm��̱�n������60[�hF�޸�,h�K9��������$N�X~�y�IX,�(��L���M'��$�ƥ��b�U�5�_�K�|�6n��ڈ5_�;�(����QAkbT4XhLaw~f,�LGC����f��m�9����s�Fݘ�'��E��y,odƀ��@�*����!�Vw8�HY��d�sZ��_�w�>��u����l�(�����8��vT����~^�sU#�cѕc>�9_?�}mQj��%�1�6��?�2-�zhqQ-+���dbY�Iy��@�Pތ�&"#���(��'����2�\�'s��a7���θ��C;6�tz���RJ\���817ǁA����bܧ��#a�vrx��,TW�m/��Ɏ�H>Zx��;D�~��}�c}�i	(�œ��Cd��+^�����;�S�a�� ���&�j�o�D5[��`��T���+-F�����F�(�W#�5j���Hm�	����J6��`��Ͽ%��V{}}ܰ?�o2�	xv���*gr�w�*>X;	�ND����W�9J��dīf�{��8�xz�o�v9��������
�YJ����G�B㶪���~��x�]]�tba@�-�

!��0�����r5�!c�,\TUtjYR29��0>�G���=�,B��)٬���Zz��~����%���y_��;Ԋ>W�{���G��l��Z\T�z���F�	�6�,��?ziH��͛d;�⡞�$H�����b��:�P���;"(z�w%�~�Y�I�[���tw�3�%�b[|�ً%M�^N�F�a�ߛ0����!k`������Ζ<���� B�
��N�^�_�{��u8�
\�܏G�>4�<�߉QB���z=��<�rF8�ɗ;�*�4��w҃���!�T�����lfF͠�F�>y�����HIO�:yjJ��R�>��4�G�{,�YYi�L�s���Lk�*����Q�y�M�#�`ٰ�B�������0>3���������Rl�ώ��]����r��l�K���?�>�`��ζvc���i�}#�J�v%anI$�dH��c��b��Ż�ک�)#��b��}f�jp�UT�q,ɒ/n�|�8�;��{"�W���6E��Z[�H�Mf, �sG�����Q���C�o5���Q�_$�M�T��S�8��[�/�U�&��^�	��'*��LיK����X��2�S�#���L��W�6��a���@b־	ˏ��x`��j���P}An��×��e�±�_W��Píλ((��n���]�/���~�%��Ӆ�L3�%�4��Fw�OG�K�=��~s��a����l�[:��4|��Гd��%�U=ߩm�\+�G��@����r��e�����"J���$}�62[N��������4D�X01)�k�Ї*�}(b4;�uRLXm���.�y6I=K Y���d�a#����z<2PKM�����FH`��v����=H<hg/ v��sfq�i�B�|6J7_��\ �e]�$��J0�e=����B���v���'�\,İF��<�����Պ���aw�[�S��|�Iݮ��fֳ���8�Ǭ�����v/�����9��<IV��?�}�o�FW���kᤤ�
��p�@�l�_7�L�)�m���Ϯ�s��̗��Yq�EZQ��-rIW<M7x��S�^�bZ��ЎR·�dRbQ��̧!:��=F�$Z��
�����l�E�ck ��G=݂$,p {�X�S�X�C���u���DC�hL۬�������o)�0;H�R��gU���-��5k|e	vD���D��Uh�(�ֆv�vN
�f�ޏS�~B����g�Zy4��1�,�}�r��Y�{$	Ie��e�s��#�7Q^���]H��asak��D��ON}	fc�j�	��'G���uZC��� p/Wm*�/
�
�]+���B)�AD~;� Wjl��̲i��r����9�#κ&&����̺4r� x�!�f¶)4�1H剙�	0�{�SĢ� k[`�?����.�s���Y	�Ջa�2�k��E�"�Ƽ��`�(�?V(���-��T�1����V'O�K����t{V3w*5Ђ�n��	TTV񶛔�#�G�s:u�Z��Ȋu�o�r�ڕ~p�>+k��p����]��h����g�Fk�sƘ���.���_�p��`�!�{�p)?*CEn4%����T�F-&~�G@m[����j�S�ڈ��6J.D�?�J$h�o8�.l�Ϸ�t!�;��c9b��v�Q���(�μ_vz8�G�������J׃u2��ە� ��w����̝8�w�/_��~����)u���ʿi~��*̴���>��(cT�6�WpԳ��_�ut3�)cx�H�6n��%���������:�i�]�����.��a�,a�����Y��k�,Ir�V�!�}}�Dgc�>��v�&��<� ��7B�����l=#���pZ�4�Y苤����G��j�Md��i��Zx���ey%sk�A�X
��G�aX�)TB��69r�3JJ)�&/Ի۵$��#r�-w���`I���ˆ�JI���i1�Ƞ��\�!���A/c,.]O���_z�7��	,O����#�4a��!9m9�Ҕ�����9�p-\^�+�tXi3ewc�"Ǌ�����5#`�G!��I*+}�<9��C�=����eJ��Y
�Ma 	�_;�ԇ1z��T���k��S�T�b�����ʖ��u��~��'C�y�fF.�Q;�(^9��������u�$Z�k��έ52�^;�8o�e��3�٩4�6M%/��[������2���y�q($&�a�j3'���V�I�#s%�Cce���LѴ\�F�jBm	�R'��|X��kl�5��y��:�%�`w��Xi�n��K�,.�EL"�?Վi~D�:_la\�B�}\�}����f���v�I���3��.l%z�{�$������H6H���e{t�k�T���d����k���h��<p/�n0�s�oB9�#H���BWJ0��l�,2YFE��z�p.�鼌f�h�u��ր�7�?��OQJ�#S�Um�)|�S_�����Z�X\���D8eu~���4�4Ǥ3��"xMi|��v���`hCe��g��!�:��u�6��aE�|�6�<~�jVd6�i��Y�U��-^K.6֝��?zKǯ[L�j5��34���+B�4�u!���y�.�����6c0�;1� O���בq������8	�"��#GB���(����!:���#J� #S����ɂ�}{$:(��,$�x]��E3R�Z/F��'��½���i���NUٖ6@ͣ�	 15a����"t\#�O�w��n6R�DJŤ�(�7p���l��ȺkW;f�P�i��?�w�%1n�eow�$�2�[�m�K���e���:��[^3�����O@�wp_/	~Kס_�y�VB�O�!r_V��D�^�K�	���;�l��e�и���k�o�H&ù7kR�_DK��p���Z�ܛ6��~����2&���;7/X"܁�D�@�?	%a.�,!B㚨c�(z[o6\�eMz����@_�{���jk��M�ꄅ�=C>jMㄓ�u��t����g�(��K�th].��Y��:�¸ڳ�����dD]��;0j<��c����y~;u1��[C���uV��|[�/GS�6�,��u�	G.�"�aޢo�-';��	�	�л�k�b.5߁J�!�6�S�����Y����S�
�5�������#�a'F�7TaJͫ���q/p�"��j�Cw�r���ra�b�Ѻq�Ý7>�.�-�: ��^g�Q����C7��\RQ�����m7�V���ڧ9q���R�,걊ϟ���wޥ����MY���#1���Ju"e�j�cS��C�c�7�iR'':�z)>eƶY$�����>�ָ^� ZQZ��%A���#1Nj�mR��u�Wvz�9o��V���?}!��jI/ �^�N��(���������X�%D;.�ad�+�.)M��W�u�~��Y��9C�~f�ť�TI���,Ō�JG���<�������S't�K�d���M�����79D����!���U)�P/�������V�����s�����ZofG�"_�����+A��ِ��S��9��/��1�p�`A�Ɉ���Ң'cX��˜�Y4N�N��6,hb��{�W����ҷGקm�z�f&}b��d
v2�nd�=���OII`������;}5��i?!�C��'�w�,�|-d#mO�T������f�d��qݯ�t�s䶱jA�m����L[.�K7��)c'
ҕ�H�OmL�4p�b���D�+ԋō� �D�Q&�R�c���zL�9� Y��a,$���o��=�P�n�n%������L����1gtޱcn�l෗+3(V���;߅Q��p��T��jg9�!���윺(?�dd�qk��-7m�}ʅ(��VIu�b��5�F��������O���\���]��A��Ԡ*��O�)�E�*�<�����w%NY�D��S�L�E��|�V�����ɉ���Y��<���{�P��)l_���|�gd�kS6Cɖ���H æ)�]�ْgR�[��Ә���u6�'�� K�4��J� ^�������u��qu!Eq�[������R������D�
՗-�T�B�U���\,TN%6I��7�83;���m��\@J
�H��4�ˮ,�-���{g��~_*w�.ݐ}O���;�����Z�C�۲ξ��a/��#���3ɷD�N�/m��N�/��rH������BP�*�����x�������c����i0CI`=���.��p���O�9drk���}�c9�)��b.b�\�_s
RG;6qh��yז1��6>��8>U��P�����ń=~W��"Zk��l �Ǩs���D��f��$u*��y�8M����3��|3��eo��t�8B����=Yd#4n��Cyk mÜX�?3&m���y����D�	Y��lǧ�|m	��M}ٞ����L�w2������o��B��eW�a|�3��*!��;֎IEu/���!@U�ô�7?��������y����'檧AK���W��)��.�'\}Fr���qrh�	���:�.H���돻JC�f�����6oC��xOgw�W��������H}��=E�#1j�gP�e�����cа!~s�}��U�Z1��K���kxp"��E�5%�y�r�o2��~P�SFj���cj	��Omx��ӑC��#�P��W�ʬM������X%�@�o�l*e"Ȟ��@�l�=�>ƫ�3x�Ciū����1�y�Ӿ�̭WzH�E#�qf����,:����;,o*�~1����O޶1� ���'פ��[�@/v�i|eR�|��WT�p� �y��Ӳ�˃�ێ8/�{Kj��I���}N����l%|��ػ��-��S��s�(^T=9x���8?�ȩ5]l!V�B_Q�,�m��6!��W�Z��R��K{
�-�?{��k�L�q^-��'}��r��iv�F�I��N1�]F�>���,`��!�����酨�0���a�u�#9�۝���I]���WO���� v@^6�H�[}?v�7j�t��҉Muޒ��~�_܍}`�b�.Y�%D�4�Z��y�)��z���/☊jPf�I�����"��T�<���->c����s^�Bm)�b�^��KhT`ĉ��MV�-HR��z1�"���m�M�K{��4֒�V�گ�����1FI`�*B/�'҂�Y��mFŮ
endstream
endobj
135 0 obj <<
/Length1 1642
/Length2 4911
/Length3 0
/Length 5743      
/Filter /FlateDecode
>>
stream
xڭWgT��V)�{U� ���{GzH��%�$�	(H�R����&E	�i"E�wQ@�z��g�{ϟsΏd}�<3�̼�|����fd*�A8@5p�����,� ����2A� d����6��a$��jH(C��Ah�,�
�C� QQ����	'@�A�� n���_�K��O��9�\�o���
G�(��@S(�v�anP��������[� Ђ¡H\F^n00@��QP�#	p�} �p�5� �K PP0�C=.!~��C�p� 
���Ѹ;@# 08��rY ���U���p�a82#
�#ah .�����:�� �en�8O�u��/G�C� @C}З�� ����r��<��_ex�`p��*� �N $��B�hpܗ��W������������`h��Q�DD����v��I�.�E� ���C�<�ļ��_�}93<�"@��@I�h\J �����O�������+��g��]�z������Ԛ^nn w� ��3 ܢ��]�\./w�征��O$���W�����.Z��;��_�SG@DBP��҄�@!F04��r�]�/;�"�`p(N�_w��f���/���Aᐿ7���W�BF�&j��|�b��r6�M���o&}��K*UU��O@W����8@FF #)���d��#��Y�F�| ���" �����N��р���2E������p	���H�ؿ��?Ͽ�
���I&?"�rO\R3��Ռ��~u�<l�GQ�Y~�JD{`j��2�Ӫ��A��&����٢�һ��������\��\-R|KABvE�i�~;czs�V���K���&v����[ĐD;�<�8�s��9� ����o���BS������zxp�mo���A��M��8bN9c��u�$4���|Np�-�<�WyD�~z4��M*Q�2R�ʺH�X[�F��e;��t�@C������~`/�_�<R���#o����f A�f8��O�"D�l�,uZ��Ｚ;^�1��\w����}����Ζ*�\�t��xʎ��u/u��$w�s�Ǣ��h����c�ҽi�;5���/��堯�571�1G�����4��q*RLk�k�Ȏn�YfK��ǎ�f�<�ۋ(㓟���V�V�w�;�]������h!�*���揽�l��s���rA!ӷ��/��FJF$���:�)�:4���`��\�)a�U�%��ޣ:f�ѳ1��P�_��0�1C��e�햟����=���� ����I&Q.�:2�B=��i}ޤ!!�ʳ���q:�V��&6�#���z~U�	ȪrS(�u�����nz�5B���m����Ǟs�D�:u��Ɏ����g��WkӃi�5��O�=��������Y�=W��y��V[���]�#���9����٥��|���x�������Wd9�7��{�iY�9����!v�z��6�`�B�b�k����d�s�XM:;[W�M֗?Y�@q�<�c�wV]�-��R�Hb�<�T4�_N�� i@��%D0��%�x����UU��Иy�o�s6������G�T�����s��}Zz��I�;y�(�q�:��!��D�Ju����o����;���g��ު>I��˗+!�QV�ռa2�R���
�Z�c�u�����#�
����H�E�;�z=ޘ���mz��?Ϧ�W&|S��e��R&�4��\n�0%Z��"�3���;�/^��ږj��UnGbG�V�;E�2��u0�}�����X�r��}C�gy�:{��6�Q4�$o�y��c�)c����{b7P�,���'�� �q�4Cs@�E��`kͳ�O<�,��E2�����?�pۅo�K.�os�@�sg���K*�nk�ܐ�{�1�_�~��~�@��8�<�Y�X�W�☣���ߥ��\�j���@[� �Uy�p�x����j� Z�g����N��b�:Mqq�0�76^c�2A�|7��ɺ���|Y���gwC�읤#R��[4�v�*峈�NgY���UZ�<�sc�p�]JT�n�#��y�9/��<X�Q��>�S�t��[Z��>!�|=Q�������Z�n�sƷ�8���nǍP��zz�Ĭ|$L��O�'�>�E6g���;jUl]���A�>H5P�R<8��� ;��N�G���Ŧ�L��RH%;��DȝE})���%f�q1�d~9����cY���?��^{�:��ip֍��s�����P$��?��À�یq�����Ȭc��Dwk����`�+��?���?�C�0��)��1��^ʱ��#-T�{]"�·wr܄�8�@LƮ�A��0�i
z��r�\4�25�ǧq����B��B�+�չ����T�sVkۏ��W�Ҫ��J�s�����u9���ŹB�>j�0��T!����&vj��Vk�16W��}g���&�$Y�þL�J�V���	��8���C��m ���|��I5���l�N1
���O����S���y�����%h�y�z�ܯD������Ċ�����u�&���T1BH���yQ�-����"�����S��\��Gܖ�^���ʖ��f��	મ%k�3�h̦^g���,&}��>"��	0���f�������[��.�~Bdm�V,th��J���ӄ�{�i7���fSE29d�C�������aFӐ6��o-�涀������g�N�"҂�#�G��ϟ�6�8�:(�I��2ɏ�+��/�$:�6�ee|r�k���c*c����E� 4eDt&��6Qw�J�T�)NFA����ӝ�z�:[ѳd��8����14�7'�eL���4�v�<�!��)���l��T��	�w���1ۧ�a���U�43�C[b�T�2��e���+|j�Y-��������$W�~O�h\ΆM��8��!q��I7ź0�!H�2O W{���wZm��V�nG���}���~7��ӳ�T4�\(�kݎz��/)�H��\��P��}��m�x�<ёda`<{9so���@�c�{� ;�2�i�V-鏊���� �R�"A�<���!t��r$Ι#��vl��f ��`�O�W������aVg�����2x��͹Έ;�X�����0�N>S#�oj��+���͒M=���^'��Qc�_3k��9D�8����BT���$]\y�M�S�:j�Fo�f��������O��٫A"�I+��P��w��}!^���b
W�]Rx����wJɄ"\]��YN�kZi�IJ}���E	�L70b�c��$ŕFx��1��2��F�I�Հ��Y�Q�G�~�i X��^=�����Q��Bg����R_V,�Nb!�%���am��V��xx)�Z��������F��x,7���(qR��,Y:��Wo|���6��p(���6V����~��'],��`�X�N��-᜗�:�_V��J��
q*-~^��	A��9t�\����|�e�\�}a�N`i�M�m�&��K66�u�u�'D<��^cz@�3�J�u�]��rg{"��km6�tɍR���$��W�t)�b��j��k�N��fGs�G��:����x�1'i0�yta3MvV���Ґި��5��3��H��RZ�ֲ������,E2�c¬ʷ����m��)�!�����'(l�u����xB9/�䎩�����Z��Z����~U���@��
�*��t5^/�N7��
#�"l���ud<�*O����6C��?�}��,�1��6[۲��'�g��`t����$���������#9����?��LKLf�?:,��K�X��e:xI�\*���!&�{D4����$�+�y�`}s3�i�z�0���O�jj��8�u�]����B��~�����V�a�o�S:�R�v�3�y\���d��vjԋ]�s��Ρ�jv����-_�}�i�
a�˛+\O�2��Ҵ�����(��������6�*����k����O]����O�W	� E%�4`l[ái�����7;�V�ާ��'���;v��WF�(�J�hB���WX���ϝ���Q�k�0|.a���Gs���YrŽ������������w�d�o�3�L;D˧}�(���8�f=vr�f��V�`6����'�q��;�|�k����;6�r`δ���4��jB�z���=�8��Y�����槽�I��	���(MX;�T�J���܍P`�Y-�i��� ���;�R7����ڃv���M/�ϤT��%?����!1AB#�n�$J{#��=comqTe���	J|��/ɬX:��
m��4��cbGD��
�:��%PrE�މ��/�lf��a�PP�j��S��:Я^M���H��R�sى ����t�r��J+$���:m������l]+��b�����»�:��*��/�>-�(!�`3�>��F� �w�c���c�#XXr�9���L�/�`�)-��Z�Is�䪠�̰����O�d��@.�(<�la�^6���1/^r��J��6�x�t��r0K��jF���Ǽ�ê����=�KO��#�] wF�?&��1�SO�*~[�xn<�	,K&��<�1���H���~�┯ӨW4r���A2��Yay�K�;[,���5P[�qz���<�/'�-f�'��?�>�`�C�	,ڰ�:T������R몑�Ֆƨ�q���K��H��(�%Ey���V�U��I;�м�~�p�'���ij�+��¡��We�.��,�cIk�S<��-�z��*bi�g���PNwc���w�R��6b"���Y��QR�^m������o��t���ݕ�z��!JR_�Q[�>����Ψ�E��sY`���Y���6���go���;�̖�\��<d��qT�����D������m��(h��B��Q���|bP��	�r ���s8G1�Po�0٬N����#���9�,ۅ<�1��`�-�/K�	�i}�D0[	����F��K9��޼[z���vA^�"	��d��H���	6e�'�~� ���̴��E���������Qq|�Q�HpC�5��l�:��ma|�u�Fv��6,kPa��q�J���3���.����X�<�}2T�Z�Pxd����Ƿ J�z������`�SM��-���+�!äu��86��-���6�>���Ǯz�?�����v"�������j@��~�v�F�W؅`*;)YY$��Wب����]s��b�Q��.�u��_T��s��9R�xa�N��쟇���s=媏f~e����ub&7I��XD"�>�«:��f�i�:���G=���_5b�|���-~����bf��t�oߜ�r}��J\z�؞><�c��;Bbw�x��n����j[��Akm�zk[e��<w�����gS!Rp�q9@�9JH9K�m�N����Y�E>=4A'M��{�Zf2ތ�:u.�/"&��r[��>#L��h����2��9�r��q��⌎�t�9ҖN��kS��@�!i�r1�Y{\[^� �>$!�7򄴭N%�3f.9�]�+�ȅfʒ�i(<pI�O�ha����;'̐��b=�����r/��D�Φ��K��%9v	 o�c��C��Ԃ4�;�#'�3"�iI�g��ch��->���� �ϖ
endstream
endobj
137 0 obj <<
/Length1 1630
/Length2 18033
/Length3 0
/Length 18870     
/Filter /FlateDecode
>>
stream
xڬ�c�eݶ%�����F�m۶m۶mgض��9u�v_՟���[[s��1�ڋ�@N����F�H��ڑ���� cf���`c%c�!E�`d��+g�!!�7�s4���s4���� �� ���������#�\YA������$�� ���C������@������������/���������`lfi��S����(D�����,rN��f )3#k#
���=���������?�9����w �l�����������[�98�}�9 L�������`fm`�d�O��6�J���毅�_�_09G{3[G�ߨrB"����T���f� 㿖�6N���/�_��ZG=3k�����?�� �f��znc���7�WNf�&��5���D�������/�_���_u����lm-���m�/��������Ҙ���oLǿ�M̬a���qkc ���N���s6��W������I��X[���a�dl������L������?!��	��w��w���K�{��;�������������]2zր�{ �g�X����|���,��w^��Z�����o���������/5�����9���ʙ9���,���_rekC#{K3k������ z���S253���������{��W�tbrbjb*T���/C�����f�7��Q�������qx�0��h��޿�	q02{�/B�����z��f� ��u�3�����������6�1�gt���N�
�Q8���%�_�o��q������.�p��f�9�b�O
i��2�ٖ4(��V������pT�~��6Ns~��-��~HP��bX��$]��zQ��#o�v�Q��i�����Fz\�����J�r�;)��]��7��du�L�K��F�d��m�R�މ��R[pvN�p��D6862<�s�w�C�M¥��tF���k��`�����P��٢Z����������1z�!�	��x��oI?F��*$6��0��������v]���
��E=g�E�
G��QcĒh*Q!���
9�������q�b*��Hi��*�fؘ�~�� D�����'��Eiw8z�C���RJ��:���eJ�p��`�M�ǧnLy���P�ͻ��$y�C�|ʼr~x�7�)���E�j���;YlӪ�'g�xɽ�}�vK�FWw�0�?����q�d�.�[ð�X�%P�$4O�L�7s���O�?�5C���'�;8���=�{�\x��kf�=Rg^��q�`�p���Z�1�G:z�S�j5�z W��,����pa+���]���H�w��
���6�R�}hOg���ܜ*t��.���@��&�$`L��"Mۆ4ۍg����X�B}�Q�j<�d0`T�)�.�&	�2�gW%��'�L�	#)�H.IO��*QC�
�!��"�,�*+$+��R� �:�'g� [4��|9&~ 9@3ǃ��=�e��K!*x8����N`��<�a�̚7Xk��S�,Mnի���5������n$��]̺	O�94υ|���<ak�*p�1��S� �{��C����������d�(�{S�%4�i1�i��8lN�7�|��VLN����6�>�j%Ϧ.���{�T~�T�D�u���bɩf�L�iF{�x;<�ӪUc 	m}��"S��"�?���+��Qb�OST�ofzٰLU$T1*3��r��l��ż���g4�MS��:0�3�o�y�K�������R�b�X�`��e�b�LH�^�c�P����>�� �(�ڝI/Q������"�+��с��a#�\�-s�O�Bd8�x�C�r�CZ 7y��N�\�U��N�D2Л#w[n���9����Ӎ���T��x��A�ϓ��$OÓ��?���V��/�J��sK~��L�+�o�O���߁� 	��*'�kK}�̷J�ea"��i�芵�}��� M����&~���E���u�U6E�_��9��Rg2�]I��������������J0���Dt���g?'�����M�NV|�Epm�~_{��ֿ����=
F�.f��ē2��Nc�+�;ʙ���4l�1��2��{A�ڳ&ؐf:ߤlD�gW��8�J8��nB�ܯ�yl�������%8�n�o��J7�H�|�MAraa�d����p0^ت����ҨΪ�xL��D�LH'g��8xQ���l�t���9�8�NR^)���;��O��z�f�"��WѶwJq�?���,�l-��Hzu�jZv�w����lR�� ��Pl6JitY���i��s:���0�}�5�Ōp1̥�n��}�t�P��+�`��K�U��8z�r�/�P%�q��7�b�����i
�j��0��ℋJ�.K�"��������}�)�|�-u�Ύ���y���=>(#�����WE�yW͝,�/�T��j;�$��c�Ǔ/�Nz�s�q���*H�bJ�r������n����mV@PS�.\y4Ք/��X���+�HpoY��u�����cU����n%ؤ��#�����!g�V����a��S��=8je��Q�S�;s�FPm�����q<v�qRJ��@�����pϭ0E �Jy�J;�3���p����r���+�*�_�Y�\l���=��ֽ�|�+�>~���"s��1���S��G`�R�h�eQ��׭�&���=�����p�F���I�|�E��2�X��y��UP�l�[��<��t(�['�v�j�+0F�5���3��l�PO@#��#Wc���X�V"_!L���Qa���C����|�b�t��$��\Y�\��9���߇k �x2�b���K�\z�I�Cϔ.	�2��s���5Y��/��GV����
��H��R�@�#�����]P�6)�4�U=�D<&1�c~�P.$DfJ������#��'��63��[YgļK�6($����g�t��(�oa_j�&��L�x_�7x�Й���?�?�����:ĥ��6H�ctńi��@ߟ�M;bk��2�+��9����<���K����yMfi���G�ŗ���⹻Z e�$����eI�{<Q0*萰�nG��cQ��q;	/�g�p��.��G.]�`��]� Y��)�D�4D��q�\��^������O����S� �E�9��݌������67q~��$�������:.�r�8�eX\f��E��}��@M��UyҜx�)��,��������&�bu)W~���/)��4�(�h9 h�زe����A�&��|�e�(e[i,��]Y���>o��cB��
D��D��lB%s�򬳠����������ǂC+�g�%��:�7gL�H����d4�z(Gcd�6&=�
��@�ܜ'��*9�CJ���QXZ�� ���Hn1����HU���e ��ӕ_����YiQ�0���ʇV1̇R�5�ue��ƈ�U�K�M�!i7KNA�A����+�-`�IY��~̣}�V��=b=(�{A��N�*3&��Z̚�/6G�3xax�6W�0?�q�w����z�s{��׃/fh.���S� Ӻ��	뷰q�;NԚ�u� ��0���K��MY����,������,(k����!����<9��$ܘz�*y����:����|L$˝oV�|�k q@����~�z:��}Iu��z?����fOfrw�6{;�����7��\zq��+�y�b���ey(�����KO���&��OAi��K����SR�x���'8ܕ���K�:9Rj_$=�K0�Z��\m�e8��V�dkeɴ���xy�8���$��G�^�g��ߣ?>�(���#�4q�&�`�SjF��	�@BK<�Sw���n�3a�6!���/T*��ba8�X�	jzઃ���$'��?RO4)��2��f���.q�<x��~�nzsQ^d��p��כ�+�EI�n[���!H~����\��=i��+��R"|��d��lYș�.�*{��r����6]>���]�W8j��;�&�%�X��,�����ڣu��~{ܭ)u��+).t�M-��O�f�K��'(C֙�V� �E�lO�{j��0@�$�b^���R_��g!�Ԍ[�N�%�֙���.8��$��
i��l�)=����l. �%!�9�XA��=_���O�ys��_X?�����5���W�۰��}!�;�6{ނ�k�˷�BDi٢�eGA*�H�7�,��b����M��{���
vNZ�e{YkT�g�-�8�RޅE�ȹx���ι佈nQ,��הHc��/̈́����F�L�%�Z�8�	�X��f�u�y��O1�$��u�����;�M�p�̭3��b5�ahO�O�����j����$��
��I&ܭԡH��A��Iإ���\o�Q��=���fQ�����3�{�GA��Um�ړ��֥H��6�<mZ�	�`�	�K��3a%Or.��i	��_����F#�*�q�# ������7/n'�9��m�(��l�k�.��}:���o�E'/SQR]$I��\�0GRb��f���U����Z\j�$�8�velC"Ĕ��e���m��c/9�cW�#��w����������!x��N��֟�Y�v輯�:�\����'�쌌��H�>&�$e�<N(T���^�x����1�pX��͊�b���Vc�j&�b��8�I�`��& ^!**Q+zM��Y��N����ԓR���s�B�>MM��`iT��jQ� ��[^wj�u�i��A`�oj�DJ��v����TW�x��'�*v$�Ʀ���eX�D�ajU)[S�4���S��xK�@1�U�>��n�G� 'Eȿ����l��I�/�6��#�~�cP���!�p�5���Z���̎��m��q>��V�a�W�f��PZ��ӿ=vҙ�t��h����Jd�6���)�k<�N�r�Ѩs����!g~��C�2����x�����r�n���R���2��'Z� �9z_��C�v��L��^�
�$!�%�I$Y#^ FMZ!�%ұAF>��E$�5rc����K
����	�5�4	��u�����k�v���YA�>SNkBFu;�g�	b$D	�B�Ť}���5�R���,E�m�mc��h�콂?ܟ�V�	M�J�?/����^�`xW�܃�XO��ȸ��iI8K�	�@�g����EtS�Y���S��(hu���|��`���Q���l����G�/'/��m^��ʱ���yHp�B�����`h���@J	�.�ZNɶBJ�T�W��W�M)�N� 	@�M���YO�Aۜ����b!�|�.;7�9�ԹT�|x��O���qWa��Mv?C�⺹�K:Kƹ�ZiF+��3~ZܧE�D�CY��^��J4|e����P��XW��Ε�����	�Ѹ���q꣪�.~5Q�k$�3�з4��8$�{ݰ~�S�����Q3�s.���c�a:��*�z[ݎ6��Q턫հ!ǁ_�:��KU��&	/�hW��+a�"��[�<R���F3�%0��-��!���ɒ(u��f����aS%d#"�g���u��H�
�t��+r/6��)aT��GD��od�<;Gh���"&G��nא�"��Z\hPV\?���/���ߥ����H��ɀ��fn����nr�ʫ�D�p ��&e������P�n�6q�55RƐVn�A��_�kk�M������;F�����^v�z�f���Z��dy�Ј�%N���_a��=����y���=ĩ��T��'?�D-���^��>V=ʔ�nIq�)���
1��1�]ϯΎ��oڪ�H@Ns�l����j���_SK�t�ě�0b�NW��F_J��U�W�6 ^�[+ů��ea
w�o�'�j�2!���MlMGw����Mjx@��|}B��J��<�VQ����_>�b��[�ғB�@ U�y�gؓaMM+�ƍ�ΠN�����#���:��*3��Ȕ[>�%;��,?��9�'y�#OSF����	v�͟&<R��޷����/��B�����kb�?`�+�N�fo⳴��P��fdh{����+�&U�_\�&�@�=���mٚn��|��9;Q}	�/4<�ި�(�?~ a���=`E��aPΌ�s�RU�h���j2-:_���eSރO� �%�^K;�Bst�mZ��nͧf��1��0��N�~��N2ǫ����"O����3%�z���ߚ�zը��
��vI9����RĤ䅔~ޅ�m5P�=	���0`ꐬ�ǆ��������oQv�Je����MP��~Q3�OFG9���a�3��E�z��C4�|�3֢J��=����Q��?7��X.�ͅ�� ��ҽ/n]����J��}и�4���fX�G7>陞y����U0	V�����me
g�;)�
T*jU��W���w'*�?�v��6�ix��ٚ�c6��ǰ"�^������V~@�����C���ZҼHFu"T!4�(��D�V�h#�>�lfCo������<%�\B�U�i�8y��`�`������,73389-jqElO���eb�ک��2o˰�rYm�˭L��	��!��4���l��$�ì5����t�����]����u�/f�A�(�k&0ˊR=M��f��q���:��p�ZO�cMh�Eure-���^��%␄;ۯ�s������B��ehnJU[vd�R눺��=w��H����k��&Ze]]`�bf�ːbw����n�9�D����>�w-~�:f���<�H�g[�]rl!�S��v��q��c�9Ia��4C��;x��$_�TL~��Ɲ�����5@}9�P� �9w俄���,TA��K�V`��-E�lmB�/��ɤ���r*G���Z�V�3 ��Ϧ���B��
�ǣ�ȵ�b�3�, ��ڀ��x~GgdTh��ۓU3��ztf�p$����w'm�?�2�џ�|�JT�k���P�q���{����h㫧.�ߧ�{�lтR*�z9<�8sI=�@���6E�
W&A����[T%�0j&n��+�d/���7��ؓ�r�m�����]��΄3�f���xh��D(g߻7��}�x�p�]7+h�wW�c7/��S����y�z=�Wf1�x���/�0k<1�B=�a�x��u#�n���I���a[:�~6D3�NB沗ȡi��1�-��A�]�����>=5��dOڢ��b:&�QB�a2���.zs��j��i���U<<Ϛ�� �8[�T?�=�l���:'��L�r"k$��̮5�����5��9gG�͟Γϡ�]�B�a,�M�@�XccY�Z�b撌�la��Z��a�/ߎ���<�䨞�p��ل��C6�7wi/I�|{d�+D滈jQ��"Oq�bVn�ۄ��bM~�#X>5�F����}R~C����G�HIȢ{цJ����,�i6�
Pg�t|��[;$X����'?�SA!�8�;��O���+j�#��8�s�����h?=Z�۬�l&.��/,�|8�?�гİ��N���8AD��+ �-�χ^����-[��~Ƀ��LR̙!v�j$���\-8�kO��:��8��P���(����6���Q�P��$��+�w\6ۀCN\~�F*<:�+M�n��t(�u�zl�P�2"�.�n1��s��!kC9XB�S�Kn�&I�+�wB�eV+Mb5r��ն��Xw&+��q�J�o(�@5������j�5�,�;���̱��G��j�l�Y����lQ2�κ>���<�W��3���2�D�_@�-lf�~�+:c�t:�l���*�PU�Lwm"P���׈�����В�
������0@ҭ�*,
uQ<%�±�Zƅ�*؊Avo�F���A�)��F�(Z�"�Ʌ��L߁=	� ��Ik�"�x��#�/0l{�r_ ����gfYn��F~x��6��sJ�_�ּݷ���7]���>_ f����ӆ���=zC��:���C M湟�����P�sO�����!TaPs�:p���!�A�����Ь�1*LV�`ޤkS ؒ�+@��s+c��>�یZޚhH��������s����U�b�{��E�ԃ�5|WY��hنo_�:i������֬�ڄ���������ȖMs�]�� N����DTF')혢#�yԞ��N\���u�[U��?�^�ߙ� �՟��*�����W#�Q}�!�/�"�C�]�g*�a��WP��ch<+�][;1i�J��P ~04{}oщ�h�aC�uQjt� #up��!T\3��c-��-��ڢ���X����:J8a�'4.(�l��O4��\�.��֝!��Ї�ɚ�HX�A,���u��M��pϞʱ��|E�����15,�I޳$�gL�~���Ȉ�۳aC�ƨ�2z��U^^����7��:_4��� ��՘~���,�t;J��~X���v�$ ׀D<�&v����5����ͱfVw {���$��s����H�򠝻ʵ�G��[�W�~�ӯ�O�T[�=w�n��0�0<ک�]��^ ��:�4�X��~g���=#�9��$ {�b�Z�O�;<�ةl�>u��nM�/[�}"��n��]����8J)k)wr�Q�;���nu�6%��k͜Y���P��TEM.�E�C��RSP�<9s����M��CM�����3�I(D�p?4^6�a
؝pQpǘ�Wo_�Ƣ�f�*�I�h^q�%�^����7fU���˪��2�8܉r��|�zJ��4L;s%���G�|�l�]��u�j�<}d|��i�`?�'`/%/mN��HL!�#ǌ��:�����%f��g��Wӆ��tX�BR���P�A�t��U�ߓu�ܘ���h���,��`�:���u�����{,1��_����8�s�0����6ߘ�ۯ.���0��^���YoPKpLГ�����áxc�c�P�kz�8�@�^&謱��{���u9�%@�_�B�qCI�)���Z��O���}�QԶxGW# �Q�~��|���? ���4K���
���
f���I�%vL�~P�42��"�x��9C������$�Ӊ�����U�^�W��R�q��Z�9�$��IPX���v�0���e"���h�F��I%R�U�k��T���1�u�:^�K���U�i�1rA:��э7�����i�L)�O��'Fub��{��)k|d�2���^m�`>J?��#����#e%�C)����IM6�
��#|�$���Y���t�����$��ըQb�Z��!�71�i���[T���c��]��(gMЬ��e�1�$�7!ekP� ZU��@&�>���"5뇅�jI1(�SBfӊu���W)U�����bܐL�}6�m�p�~�-���	�#M
��nڍZG3�4ZL-(����n{�k$���[ƅs�/ޕJ�ѧ����.e�<�~|s�g��xPq�.P�41p��6����M��i-�g�<�/��'s,>�Ar3�ӝ �]c� njN�|���T����i��O��!4�oo����������O�)ƽ��b��sv���<��D�)���A�fM_�l�>+FW��&�s��Ab}RQ�K�B��#w�o�'��G���u��#��ڴ ^,X�*�$��N7�0l��a�����*���\�nC�ъ��ƶ.�$HU�s���'!���kG����X ��R4Hg��!:j�X�6�t%�=w�o��Hf8�B������Cщ9f!�̐g�2ɖ�a�r�1�����S"�Z�+���٨O�4>�nF�Y��g�����C/>�����4�f⽚�cS���.�������bܚ3�}}z�Gw�����ÝC	��B��"��Un��h�2�Z�6y�w�ר/T�<V�+�ʶ���7<y�˝]}݅o 5�!���`�-����rs�oI�T�ǈ�Ƿ��8D"L(}�K�f�v&��q��j�q�g?2���K�P��zwH�(W7� .�V����@U���1�.���8`�����`�|Z�yq�n����ja��%��
d�_�e�R_�(�}I�_�V�p+i��Lk\��ѻ�Gz���)�kޒ��o~v��"+%�(��5_
��j�,�M����;g�����3�;�����w���3}{M{�)\+�Ck����Ԭ�NU��a�����jc�����
*����e<��.~������Ӫ_2I�*�,�]2�;����WX��'�[�x+�$2���K�yK�I��pc��@����C遛���iZ�ʰ��K�^�؝�f��'��u-��W����E��h����(K��M�~��p=��b�y� D: 6YR'v������SCi'=��4�_�J��?D�Ĕ���X�w^
��� �����%�2uPrU�;����U���h�d����5L?!��(�I�FR20?�-o�W�s=��a��-�Wq�����%�ț�b�A.����)�����������w�����O8]��}��ۡ\�K�zs�`�L]Á�V��ss�@z����Ml�h�q�hC��PI9G���a)�!^��&a7"p�a�3�)�������5�����^���G5u�����Z±�f��Xq�+o0�FV���{V΀�=�%M ^g'p����1�r����33�cLwdĤ�����֗B»��#��nX���&A7�=A3�S�uΕt�$ �7�GM�g��L��|�3or�lխ���B�&�8�Wv�îa{��J���v��X�B��Dbt]�+M��w0"�����6r�5���H��G�~^�,n���y��"��j� a�-�;a#>r���IS����r��)����5�4ro=���W!�8��Ph��*����K�6���A������.�Q�"h�%ER24��έ�r�Ņ�u��g�����7�th���7��`Q�fh�ɬ^f�g�L]��$��i���y��2mߣj"�u՜���M�����/D��n�!nC����οr}6�M��Jб�Z��/'w��̼��XU~���^�����ݨř_9�/\B�9
t?���b�@��b����5����+�c�Y%}�\P9L-�ˏ{�����t��쨑���N���f�<J6|)�ј���Ճ|#��d?�F�>n�w���ci�N�/g�*_y�\�!VƷ��y)U�!Z���e>��0�N���l�����Qf�6�8Y����4]��N[�.��[F��?h���8��G�PblQ���f�Vĸl�;+^Ό�)�A�3Ŋ���|��p��C$cd<���+�6��x��G=ƌ|�k�M8Vu�L�+8���� �������z@�7ve�rF	����*b�'���[7�WY��,����: ^��ҳP/�O�w3v{��\tL�!{��[eFǸ��2;	���t+! ��,q#*8���s}J
 .Ʒ�z��[�Ɵ˳�����L�y���
�r�Z:���SΌ+�< �q�\�� �ݜ�f-W�K�s�_10���N}�&�L��\�@JEs�=�bA��z��+����A��#w��J�Α-/ٱ��i��~V��Q�䞵���/�,HxC�u��
C�E�jx��O���a����5n޾��/�"��M\p=X ���*:�cܘuʢ7s�-�X������h�O�R���Q.�l��׷�Z�9}�5��P�Ҩ���W�����Z��5�^�G�k�+���ZP8E�fY��i|�`��\I�����p��]��CN��W�|�:�s�G���F�%T�̇^dŴ�u�1�s�xݞl�w�d�]����rĀ�m�����_����2O|0h�Y֣��p�|�1��Z�n�� ����5�_�����ѽ��N��
Ůa�vl�(#�ǽ�ͨ��0ï��!Q��Ԅ����'�-�р�(�6���k4�,?�@1�t&k� `i�:�v�e�|���?h��s��t�����,�?��M�g#�FP��2C-�1��e��Vh�+׷!�L �����0����4�����B�d���q�8�7z��,���������/����d$@�sJ׍����\�;���g��w�N�b.���IXބ��8<Bw0;�3{�S5�%W���EȒ�2L�e΂:�H��mI�R�[F��e�Iqn��12���َ�<SG�/y�.�Wn��]>�31���]*��%�bEy¸��kPǬ�/�+)�d��?E1�����aPe�N�e.�I��5��!��s�j#f5��<l�����;T���z�l$���l�h��hأ'ϼU�i"�����	uI��c"�_�Ȑ���L�!�Y�08JZPZ�-�A0՟�yF	o�b4/Cn�>����y�^�~��|q�=9ș����
���+w���~�םw0�t�m�c�k�?Fה!-����pj�C\S�VB�ǧ�����Z�;D�+�&��R�V~n�h�����$��ߴ-7�<�6X9��� 7��Zŋ[�g����?�JTȰ0���ᱶ�yո��0���H�or�@�w�Mpy�����% }t��������U���_��|'�t^��uAN$Ds�&���U�*�S)E�}X9UI���#x��1��>��$�w���)�p�'�������!8	��?e�����6���s���t����ǝ�l7�8u+�2��g�绸�,ـ��~�0���}����U�sP���
Vx���v�Tjm�<�,��y�N���!^��[���G��wH��HyIE�����,�C����[�T��cDe��4B�Pӄ`�g�w�i.�vN�)*`B��F��VrR%~6
��Na/en��"N��tuzq������_�Ļr^\���Ʃ�����Mb�f%Pg9�j��8��$�w>q?#��-KH����չ]�	ZE�F1�����L4�4P˽�cjE2���Ou�յ���d�ޛw]�=܈{lA$��g��*n�,��R�)����!�V��H�y�e"R~E���-�������T�dK7��0�r�ؚ�������n�T/��3���������ц\��Շ��)�����_ѣI��\�zjk^X���?���Y�x�~a-xh5��p����[�lgo�fRcL��L��-��5�g@vMRXR}�d��,�0���8��bz
+�@���-����6�?�4#��:���ډ���jИ�f)�������e9��#d��D�+��3ȗ�UΓ7�+Kg0��BH��7�G��1��JBA����f:�S�d�������A�Za!�=����&gI�� lU��L�o�j�T�@yR�eH���-B{ �^�S��%���0��W�W�m��P���l6��o�����dg���w��Irnii~�!�L1u�c�f��W�M�_O�ϐ_t<֡*�o6~h�τG��!���4��"����.=�3�����w+�w�hq5~1B����[3������p��D�zU�smk!� ,�a���I� gs�55�'��%�ὓ~4�sq��(��a>�<P��X�e���Z9�� ��z7?���<�짪A�댷.�A40�ƠCĀcro�X�&;��v7�L�O�2�k�Ʃ<�����O����g������<`��"��������2�@�@Ήd6Q^��e�񐐍j�m޽7#�p��r`���)&ӝ��P��V����u�{R�@�7�&Z�Y՚s$>Fa�߆U�33#q!����	��@�=
Wpy����%Z����u��?����&��y�}�j�� �'ә5;ɟ���5�.Gw��bJQ�,^G����y�jə�2��ۜ��X��6��AnڊLe?�_$jM�,��N�>�[�?�'�<d�J��6m�k���"�r��_rC��1�:u��o#��_�ˁK^�\��Ӵ�a��+Fd���o<�ڱe4w���N����I�r�[,�8Gx�e_&��_?��u�#?�?�`A��z� Zm�'WD�G�nK�����1gdP�n��e����^�;.F����/_�k+d�3�Q���t[�7���瀴�g#�AB��R�>,�M�oHBS2�`ѩ�<຅��3�#L�8rN��
:ʈ�ڑ!�a���*�z}gH�~k"IJ�b~"�\"�z6.b>et�2%܅�o�A�+e����_�S�o��w�j��S��TfS�\�i%o�Ӣ���?h:�K��R ����t�#Kbh�3ij8��e/����n�x��r��dK3*�%�sE~~$�%��6��ʌˤ�>����w��O�Z/�Y���7|i�w�b�v9������QI⨽��o��b%��x_ �I�]dƸ���m�Փ���Q2���Ȧ�[�ւ�;ۊ�����F#>�T���Ģ�*֧���Pt�yl`����FNW��3�F��z�����9C�FS�+�����ͭP��.H0r�@}{�+1�z��d�-I�(�@�'.U����ױ��&^Z���z�1~e���g�RX��g6I��
��~���d�ai?R�/�μk!��ez��|��Υ��:�w����8TV��=%�D�4Y�E�}X:�	��$6ށ���V�J�����H��臼S�<�h#�v�]|T�R�S��{���������_~|(a�K8�*Y<�H��\ݱM(6P��p-����dǂ��MC2�%?d�ܫ ��\����g�>��_#�l�Q�Ѽ�HY���*4�Z�6(�IВ�3w~�(� ?ӏ�n�(�������*9�1�\46�g��&C��Z���s�vr�C-��C�������2xC�g���w6
]	u��2J�)��Q_���/d֍�IG�Xe`P0�h���G)��Ш�}>�>�<��Y�_@�d���Fo��rU~
��/�P%�1�|�ūM��kחݱ�K]�����Z�����5N�M�T g�z�"C�HU�)T=s����'�.o���;7�K�*�������Hv�@w������c߉��nw��ҟ-��Vl���j�	�4�V��P0�w��
��M���X���G�R��|���n5����XFI�^h �ax�:����9�K�~�jL�X֨O������乣�p�m��x<N������q�N3��6!5�Ky�y�Eh9��b����6�;t�_� �Ǔ�SA-*{�0��媠$C���D^�A:2�F1�UN�}�؀j��_��tT���iܳ�$��'�O�g6;�I^�)AK�����MS���+�q��K��;+��������/�U=N�c��ê�	T�04���yEU���?/gCHm�7�C����9�2s]0���.�I�xɯ�«y����.�H<��Tߑ o	�+.�X���o���>8�R;^���� �'\���Xk�������$����5�XJ�������K���X�9ds�Jr�2V��Qz���)�w����vv�QS3?T?��Fn-ZZ>ڄ�L#T����׶��*��V��襣��luG�X���&p���w��ł��:=C5&3I0k�Ƭv��y�=0�����6|���a���:�;|��������ļ��J�qoW:�_��:(�{� �ar㿑���I�~�;�[�>�ɦ��.�/A��D>i�n�b6Yn��{���h�Ĉ�Ҵ��G�1m��1<���#�k��};�,�"�E���~=\��7�:�'�3!��h�b�{��A�4$⫃-�*���b'p����)g��	�c�flw��]}'��`�	?	�1�Ī�����n�Hp���_�mب���ƬP���F}�y��0��%�X}RC�`7�5�EӖ�^;���R�A�[���z����Èr���< �� ���f+���=��,����V��{7	0Aq�#�Ĭ��`�Y�������N#��4J1�g�����L���������	*�8�Y��-Ro��#��O/�?���"?+�@,� �E�jq4�{��t��R��;,im��F�v�,M�Ar1��ͰS4��|8n(��ˢ���0�Z.�y>r�*�:�m�?��?lޮU� ���JW�����ulo�rn�km��T�'}�*��I}y5����N�c{e�T�\�[	}>Jb��i�� �A��|19�7�V;p)���>�P�t�F�+�T�G��C�o��Trh��[�Q��)D���?7��3���|����<��iȇB�TVJ�h�-���*XpsR�����&x�9;#�7᭒y��gA�sk���	a;�����|�G�y�tL��?^,$���o���E��,@M�����R���ɷ����O����6^���v�B�;�-Y�-U!H���+���SF��25��oiS
��� ��HTʳ�wo�>�/�v_t�#��x�xe�������^f��PQ	��f
0�_���e�f�QU��zA���m-�>�H̬(��Po�d ��'d$�xžuĎ��!	�N�+N��Tт��]�FZ���iJ�eJ�"�0{��0��C��)�{z��B�'��@��]�|�2��  Q����aa�)�+�b�����㄃�9� Q�H���I<L�|Q^&?�À!h5~3B	4*��;�*�.w���F��t�(_�E��[��a�r=�~������Ba��G�K��G�����F�B�	�"�[�W�E
n�?`�A���7�{ �z��x��@�E�?���?k^��V~��w8��'?*�Oi�6�hQqH`�H�K��u)��X]����f�5|�	���c�!�M��=��s�r��
Vb�p+���=H2z�#Si����j�:{;��tb����i.VP4�`�[j����/m��}r�9��Y�I��8�������+.9+kjY�~,a ̃��r=I^h��e8`�z�g�3�����m�'��n9I��RWn�]��P�T0(`��4{O��Y����J~���!""�裋�XQ�YEo����k'SZ��I�*䄸�$1�����xv�L?���-£�R��N'T� +r�XP�ʷn4b�<��ȃ�e骺��)�= e	��ޝ���SA�7�C�g"�jj@���K��Q��U�'��� �{C����4
�)��݋a��q�ꌵ�6�D\��s��PY��CT��(��o9�	7~s0TM���~P� ���g���S�ҋ@lX	<������-�f{z�e�v�W�p�kK�5U�������_̉2k�g�b㎼B�xM��r.*�>s>�1v��G7A����lP���;�&�\���W.��|f�6eD� n<���e��oc����c��_K+4��tT�S1��S˩��)�ڭ��Ń?�2TA�z�}��N�Ձ񁶌8m"�p�*�0�[xn�I���Sӏ���~��
2�d��/�YΎ��$�b9UZz|�y���O󺜻�M����Q4.��48�$����$�m�HθJ[-j���r�࿑��K�r�m�*L�>��9�����A�H������.Ru�qק���-)U�3q��X*m����kD��c:�Tپ�r4ȃ"��_J\ZRO���D!��Fy8�V�JF;c`�q��CM�q�Կ�STr��f��ԃ�V�u�CY�'�o������-���/^�I��E R���7��fV�O��.vf=�V�f{^LS�3^ą��!�F�a���(V�W��}�o���%'��.���˵e��&���jC�Z�3���A�6;0u��"W� >���O	U���������U2P�����-�x�� f5{pG��h/hi��u��B�٨[� o�i��Ŀ�)j\tV��>�(?�A�!�Y��U�?�b��3 ���+S�f�`�V'�|I\o�&5!�) ����C��㈀Ot���j����v�İ0��=��csiǨ7�'K�!�)�(g�#.`p6�w����f����[���x�V�pU&Y������׸o��"�|�#�|y��^Mkv��|�G�0��A���^M�Py䆈z�;b�'��o�b��Z�KR_g��0� �u�0�>��D�=8��5є�D�wU܏���%.��O�¬���3a��{w��(���5�[¦Dyq�;��X&7.-�F��J� ��?hW�Sm=��c�C����UmzIƲ�ȮN�Ƅ��,�s�'��/�&w�!B����y|-�فlc ��w&�^sՑ�.h��V�+�K�{{0���~:ʬ戮əZb��]y�¹!l�R='�FW�	��lͷ�k3�
f��R��ձ9)9��2�E��b܈qnR��!��;�~�Ibяhwi�g�f^OX���"򏜺�� �<\\�b��(���E��i��Fς����?������Ս<�^O3k�X�dI���?����2�¿�Գ�\�����p���Ӹ�Uʛ�/l^R_��Vǡܒ����F�|�y�n&{�q?v�A�%��2�&aCs���֗��-b=�}q��&�`���8�f⋁�k �_�31�/ �>�	2�S%_Bx��d �W���4�]
endstream
endobj
139 0 obj <<
/Length1 1647
/Length2 15025
/Length3 0
/Length 15884     
/Filter /FlateDecode
>>
stream
xڭ�cx�m�&�c[+�m�v:Z��t�tرm��c۶���}gϞc����?�����YU�YU���ȉ�T�M퍁�v.��L< K[cWg{[{n9z������௎��\�	h�bio'f��hMb@ ����� j���din��RWѤ����O�?& c�����t�4�P��p��;��\�B�_;��  ���UTҖV�PI*�$�v@��E(��X� �,M�v�@j������������?�93��v��&�݀&@�Tt �������o��3���������,�Ll\M�I����_	98��������d���l�d���UIL��y�X�����`o�������������W�bdi�pz���0�tv�1���/�����pu��3��� N@s#'S���_�������	�ߪ7rp���������W�.�@3Xf��1M\��6���e��_�����L����:���������g��&adjog�	0��2*ػ�	���c�῏����[�o���������6�����_�%\ml�l�6�����1���5 9�?����	��±4����Z�x������&��Y�O����wa;��3�3��[l�,a�4U�t1� ��������L�N6�v��$��~�:11��������?l��[�3��5���_0jK������������v�����?#i�ۛ���?P""����� zN����w�Y�|�����?��F.N� ]&&&f���<�y��/0�v&�������������_��&�NN��6�[���5@��ve�ބ7�*5#ͥ+gh\L���|(ġ�A�0߿ھ�/5|����&��q���s���c_���O�ew2�2߇��7e����� �Ѡ!�L3��ռ�����θ��A�;�d�ӷ�'jR�|t�GD_���X��ȍ ���g	�O��#�C��7���x�ٱ0�FX�I�ĉ.��N�&���n��p�?��>\I�HR�E�f_X��:J�&�DR�V,�����-D�Ia�!�����1&�����}�HL�I��2��nZ�����T�����F��FZl-��T*kp��"����~���yYK.�Cy���D5���Ev┿����G�0ݏ�����&������z��y=!y<wQ�V5c8�[�g*��Z���z�����TA�B�C��1����a|jan��3��>���.�^��� �L��zKR�?8�Ќ���.ށA^ ���!^;}/W�&P�΀u*�*`\р��0�4��M�ߔF>'�"B���O;�T�<ɜ(n���"?L�l�-X�/��b<0�x���&�ƈ�
g�|��뎠��;Y��p?��K�I.S�ۿ:U�y��)�	�3�1�8:�A�OttgAi��%:�{)A��DN���V�|�wXb�����9.B��wG����z-W���BmR]~�֊�'V��z�gMN��/	rE����� �RF����V5֊v{5a�ʓ��Ο�������;����� ����� ܪO^�P'�쪒E���s=P��ä��YM51 �n�o�\>�?�ʃ��4��h��R�ճ ���$��q�ކ�� �L��ߩ�n�䂆9�f�Toz���Hϑ�]�h��i<!�|_��c٬�&3p=	��ۛ�^p�iQ��6$��~�f*��?	";EL�I}�#)u�qUѓ/0j'j���3T����܀�*��9�Ś��c������_:�𖤱���m����U�J�8F
��EL�
��X&�	��G��GwD�.���X�0�]�NP��!�#+$��8gy�w7f��9LH�~�]?/��x��p�K^�.�º�8�ԟ]b�1�����[Ft���i}�i�M!���r���y�c�4TId��T��D	��ؔp&gS��1�5�k����(	ц:����d�]�� �\3�cم.L��� �5:�w�(M����l<^P#(�I��I��1Xiү����4(�R�X"]�����y9iEP��-lYT=�Q�.ă%κ��Q�k^'ckЭW�';B��Ro��؅|�����g�I�'wRx�_�,���웙+.��
�����z�*��eS�5+���,̻Y��%�NlɎ��^}��5�C�~`�)򦒗�F��WA�A��P;�?�q���Ǐ������X3����m�~C�t��iSG �T��=E�D���2���$˥�`�|c���MU�/��V��h"P�A��^��`0����7=��c�LUv����Wtx}':;t"�Xf�G;?'��DI~ �~�dX~��V����m!��#�
��.:,�B�n���Y��&O��[S�4S	+��c��%��הk���J��VD���K��� {�ӫ��奛��O���V,������>!���T+?v%7��f����T>��Z�b;�:���d집��0�z����ڸ�nxy����d�gB�i�侁����ʫ��:V�R����=��c.��T$�T�
����&�(ʘ����\]Gx�٩JY<��)1� ŵkYr�oA��%�Y������}Z?1'���!��PLJ�	HG.y�:�U�B��3�)�6�@��ä#���a ��!��[�&>�c-3�.j.�톼��ƴ�h���wfӚ&�	#9T_p�`n��>?�H���Cw(xN�^	�0���0�{��Tk
K����g��TTB�d�Q����Vf�(�7��u(�hqy�rE���D�4D�09`�p�>i�>a��煥;��?��i�ɷ�L��u�(��ۀ�. �k�x�;F��[J�z����aQ�l��ȱ$bY���Nѡ������rB��=-�N���˘���u]h����o� �ɩ��ȉ�
�1����~|�|(qdp!2�Т���ԜT��Q�@�l�Cz��$w<���1m^���{�i'��/8y���6V�s�u�i��X�C9#Qs�8zY����:xr�O9G�R���d��X�ǀ�&����/�c9L\V>l�<#�X�W���A�EC�Y�d8���;�%9���:��/�+�&����1���"�,QmG|����DO�Cy�D�(�q������w=�9������7Cϔ2�w?�|%N���?M�L�Vƅ�)���Y��������Ѩ�����l�ZJ��
��8z���ޘ�����N�ۙ�s�)]��
��6��='�䷥b�(_��U�,ǃ�<W�S-�mo�dQ��kn,�-|��o�Ms��SA4ނ�^�ǣ5;�^-C���0��ð���%�s�;��E�ۯB�J�mKp���G���^��j�m��j�Mn�����7�/���2��u���6"t#��_�q绬��h�"oOM�g[��,���=ٰ��������ܿK�/u��ز��m����j�P7B�Z0�cJn���b�>Xo]zC���B���,`�h�v�Z�Ւ�F��m緺�m�EQ�����MfٹO"�	���P��#�c���͖=�\1����CKV�V7$<:��%���e�&,#ԥ��U�Y��Ms��@�� "���,�6�P�^���2��l�ӗ�e��e(�t��A�� 9HV�B)l�5_��d�7�uw*���g� ��L���̓��\��aQ�my �l5�#�F�O�/2'G8�9�RM�����9CF�|��t�E��pBh�Bq�h�Q���)��E܄��zt�p����[j��E	��п��:a����p㲪��<�*�	���2P��+%&���98�����
T�0�g0�w�������Rt������2�~�j��R�k4V>t���&Z�\�#��t�����P
��U���E���>x$�� ��j�Y����Z�ٯ��������c�qȨ6c�I�5#�9�	ͲDdb+}��R�>CT4&��8U��y�7 C�.a>U�\�5+qu��7"�"�q�X����t��6z�݆� x~ڱWB/i[w�����F�J��%���;G-F���ǀS�q�њF�-�����X-	�ʶ��?�vnBy烘.6��B(�@$r���Vv� �s�\��5��'��u<2��kLԃ_Ъ>��gZ)�%�U�"�< �I�$%M]aF_S(ݛɿ�7���v���C�\���S$ZXK�P�%�;�Ox��������:}g]?���jʶw���	�/����
���j�O�1_��n5�ħI$����g��s�P�	X���w�q�?E���8I���ظ)!�ú�+.aWmݫh��jͣ���d\��5��h��ks�JoE]{gwK�wN~���Y1�[��ץ��h־�d��u>X���]��@K�)�p�o�3%�O�(� ��6��)t�8��gh�ߜ)_/��%�Q�7�<{�#E�O+!\kCj���(���n�3_�˩�#�)gJ�}z:��-�ajM��ř����1��������L|�&�UP3݃��&�GHp<�(eK#�M�U��q~$F�G��=Z�]��ж�p��z0�op��Έ2i#���6�7*��}�`�1�;cJ|����t=7M'����J���_d��!���c.��Z�Q�p|۔�,�l�g��ñ�g�u��Bk�ԡ�4�J-����MQ��by�Q�"�i�w$D�D�0�q�MY��Z��c�r*�U���sr�?K��PJs"ѭ�����g��q��BQ'H���Y1�`��M�FSA�:�IYj)���3iÙO��LX�U�0���^�}�,�FΦ���.<�v����y�2¶�/Ԝ��sUwb��F:���؊�9����i���M�ڝ1{�U�����<�p8�����-�����V��x�6�ᵞ�W���j��y����Ħ���XT��	�B�?Cm�g��w1#���>x���C�S.o��+�|漜�oU����������F=p@����
S_�s(&������kJ6ze�ɷk�����%j�=K�.��M�b����&�Z�݃��qxe/���ؖ2�<�@�Ӝ�<��D��[c�y��+8����<;�e4rQ$Y�}�v��N�&�B<v�*Y��|��\: �T��S<co�?�w�ĳ���b���B�Uk���qR[z�%��#� �E}TE�ƤԌ�#��[ْ�M�v0�r���qX�ʡ���3&�ݕI�
�
�E�di���Y�ʸ?F�N����f!��<�N^��/.-ʖTx��ʺ��.Y���]�����Lo�"��FnzG>ѝ�z��C��3zbD�f�z�tiZw�|��,·7o�lQ&}j�A�K!g�r��3N6�1�Oı����8x.)��Ι�ǏǏ4]�7�S�-��Xz�#Q�7/NCMEI� )Ȏ�V29
�li��5�튥a)T�ƍ=�m}�'��UK4/D�=�)�_�:��u�N��1��{����(I2(��<�{T@1�3��� ���?j�1f���X�Ńx�!�0y���O���6_
����|�4>l,ZK\���c�]�GCCSg�-�`��E3����m�q���L��p��jDeKo|���&2����P�i�&X�~�[��_)=kw���5�>�y,*�������k����Bq��g��������v��yh��ƨ��X*Ǐ@g�򵻩EN��ϯ2�4ؚ����}~<�)%��u��N�[6e$;c�[��W�!^��q�HO�Ӆ��:�_�#Xo��F�M�L*�v������p]5��?
�R�Y

�S���$P!�#	��(�Ln��y�Z���kL�ds�q�>�3�o���a(���`u��[RFjz#��&6@����*�9���{�V�baQQ����l�� "�;�����œWdrfZ���F�<������\õ���eDT��C2��p�"1`�f��{�\�6
Q��Yg����L��&���צ�\��i���UF{C�5�C�OFF��x_0����Z�|�dnb���*O�J��E��?JT��f9����Rnׯ<tS���\%(��n��z��}+2_�ly�0��1H)�v�Ip���0�SR7�`٦��!�\�2o/hG�?g�VGE���Żp'#�A�OY��'$ΐ�����h2��T3�������&#f�H7��^�0뵽@}grW�5$o�a�Wv�\��Ұk�uq�T��h�>����*�~x4i.we�Y{�,��ų������C�¨5~{�sc�:`0G7j���֫�7����`����OA�<&%H�1	����(@xM����[H���#���;*��-݇��x��VpEYҤu~�� uF�gr\�@7��bs���\�!J�m��gS��?I�ؤC}=��!pv�M6g��~>����@),���9jHR3�t�}tP؉V��e���)�jF|ȱF�ο�o�@�{"L���E��v}X+�X�o���[��	��|��I}>�-�J(�a�`��%'�#���+!��@���� : QK����� ;yh���]�,WV4a,��<e&�(>e�����yx{�՘Ë���S�:`4MMt�}1���v=8i�Ģ����ͬ�RX�o�&�h��M]7;��B�8�m�yB�#�Z���Em�D�I�C=ĭl�b-fL���lܵ>��[�i�gI��*-s�Ⱦ��X��;���Ö����]�����ی�P mmdg�s ��5�~�ў6�+d��g�¡]4<���ӈ��<����uC�׿4���}�S�0)����E�H/�1�3mćw�����$OJ��8�ڀbi��\u�ݘ�{��]�S�J�i$'ޔ�����1��g �j<��������M�xt��N�}x�'/dI�*�AWK'�`���-it��/�vW�)/����¯�)�JAӌ��J�\����wJ^�d3��\*��!���X�G�̭_����S��C9�
��M� Bz�<�$,��11�7:yW��� q�$p�k�=�KC��-D�}Ք$4�Xz|:/���pf�b��"Ruڻ���D�����X�7|%{|�{�&p�������]ԅn\�Q#��2l
�$�����KA8�x-�?�r�a�:�"l�#ͧ	�L�G �|���R������@V91��B���9
S��n!�o�XFL�����O�$XF2{����?5�$����Xzm�ߌ����V���E7p�Z�������+S@��6�y�Cg^����6M8	N3�a8V�e�S���su~�\0�LO��a��-��,oεL�<�A��o\���NK�۟��*���O>�ȼ��c���@ЀY��ы�jj�(ǭ����4b(��e�ݣp�j~h�k�쇩B���!��dn�0�2��kW3�ΐ�p�1�QL��VD��&��:Ѹ�`�sy�oD��2�dß<7:���<�xy+.��o&��Ƕ��Xj�q���a�8�+�?�K(�`]$�=A�i
dIּ>�����Z�Gd��Kb�x�희��T!��QQ�j�b&�q����ދx3Fk�P����*ˠ��]�b�%�t���9��d<����&�������\�E{���4����1�I��	%����S�G��r��������-CW�0�b������{)�?1a7���{�Za)e�-��|6��m)�i��j)���1t�����v	%�)J��8-^���+��>�G���
�@���ڻ&"��BN�_���D�UF-+�c��.��>x���~o��(��-&Sٯ<ם��
zs�2EP�8��^&{�Mw^`ьc}��v��"������Bm� ~�d���խ�I�b���ۑH����y1�M%���ۻ:�#_ySa����Hm�}"�Ƌy�b�f+�\�����I�#<��4�%�ZW.R����oXF�GG�1�$2�ӹ¾��P�S�!�*��C��h�f���𧜒���;f�=��?3FS�-�U�P�hC(���Nm��D��K�k��:�iM7��;�;I^�JҪ0�c�6��������5�vj�m�[���ڭ*��|M�>���z��c,ڐ��,�H7�$I�Y�f�oÆM�=+�%Qj�ܭ{P��1n������8RكT���'J���U����N�����=�ڶǠm�<gq���U��N���qK/q�9ŕ��0H�U 6�#�d<a�`_���7�t�ru��N7��8�ʹ���TdrjVJ��H\��?s3�c��*�yG�MPM� ��} �3t��L�0j/	�|��5s����n!?V8��Z�9w����.K�h:"6-���ڶ�j7\&X��@��b��UJ U�m�+zo���F�����4L.�㮟~ft� �6}vvɥ�Pz}�'-U�"�5�=�")��i~�
�O_�{��i0���}���� �\X:	Tb�S"b�g��k9��*Q�!���?�p7C�c�����hBU9Aͳ�c��<K��P��{�&r�rp�`��^6yv�g����;��G��*���T�i?��a`j����܂��u?'���{��x�ΔWŻ�x��T�
:���-���U��Pnɭ�����|�P�@�A�F�Ja��Dc�۩��!+���_O�^4�N��mL~�V�<��߸��e�w��w�1pGZ���H���̘�j�|�|~O��N�{0�nWk�
,����.��g]{Ӡ��xt�t"/��V܃�v�*�� ���L_�/bq�2��L��bE�r���yͮm>5�<ĢU0�^��M���k=15�!�WXC�]w�KZ0-^tЉT�t�>|��\�GN싡�<�C�=��;��&&[d9	n _��W'~�k-��t�*oK�6�U���=�Q*@����Qj�����(M��it9���j�ʍA�z��x�dvxP	�I^S9,�me	� �n��@)�a7���)�AyjZwߑ�Z�]�ʯ��uWj;�M�{�mh�F�%D�h�u<�Ϧzϧ��.)����FJt�8c�-u��
�	��@���Y��å�eYY��
��m�j�eB�\�� ��͘?������t�C)��0�x����貃�q4<|8�f ����v��R��Bt��%c3BY���f�Ył�7-j��W�������c�~�1Ҁ��P�1�'yT�FA�;� �r��RV2�8�
�K�:\�RX)�
���W3)a<��iN�V�����Z��g���xq��;6.<�*D0�p��������뽜��6�>��_�g=�;��9�������&%zDv��m�#�ۂh����{A��̛Q��a�����U�3��Hu����5�/��%�c?_�XVvxwD�n�:�c_q������ �}S�XQ�^���Qƅ�%�����v�r.0�[M�>�i`�IOr��P�+���8:�yӒ>	e���EP[�#�����=��L�8I:�=�����u�JjlR��\��!Ĺ�<�C��7	���D^��.���>�b���ƀ4f-�.�5���~��9˓����^��䤎��4�M�*�S��!��h�o棐��$Y�A�Xƶd����	TpGt�����ނF�h ��v�nTy���Ȩ���[ȴ�>�0��w��<��L�;��~I��51� ,;*{�K<���jҌ�Ce!m�U�IBW���P�~�q�G�A��o�C��e#)�~�*���I`ޣ'�_/9u\��x�씁���Rf�j_O�'j�D�#IC�<�~��Я���w@�60�>��C�W�u�^����n
>�B�6k�66�f�ə�;BTTFE�0��|l��N�tU��=�h>|��Q1�+�X�%���jZ6�q����T}{��� �Ü9�&����U���V����3O���?���G9�-�����K��442<�z����3������oLT.+�y�w�H��_W ��q�l�(V�� ��C$6p��������q��}�n�@�����{(����_Ԍ���\���>�6�F.I��xg���/��DWҺ3	OD~lnj7
��Ǆ{[��p���S����)�h[��ǩX.��֝�&�Ԅa��ŠY�e���u���=CA���v�&�~��7V��.�Qʋ��rx�\�LPK����v�q�$���E$6(E�"��8 w��,ut��gv��|�Λ����~J��7���q́	��k2�#\Vܸ��7� ��Ob8�v�Z��a��kʵ��l#)�������N��>,��Zr�Tߍ��zU��z�%�ۿ�������{��]A��|b{O�HE�����JԄ�"��]ʰ�U����89���������r?�eBjP+����s���9�U�y�c��)ݷ���#�<`4�ΞH��=E�I1(�z�M���P�>��t��1��}5{l��ZȐ�,T�6�e����ڐh���*헨��'�+���z2�]��a�ڂ?�� O�g)HI4����8�o��?����DQz�f*F傸�
�-�_e2�>� �'Kԡ]�yLkJ�e���qq/e��9��V�ͽ	Q��H��*�1�� 0yQ���A����f&�Ș�ڠ:��Y�j��:�N3
�۟p��M���(>�O��ݝ�jU�_�SՕ��9�F��j�����JN�izK4���u�VFT�D2/�jGy0��uo$�ŏ�5�A0��@)!�;���y^}e7�~�$�v�y���Z~bG��D����^��xYo��w=0ԕ�v����+u���-�iL[_R�^WIggx�}۷2��f�S*{��y) P�����K��Q�]&��{���,g��-V�u8O+����Y��NH�/zΜv?E�6LR1/�Eu/�"I�O����mD�llj�4y3{���=�d��������y�ss����a�> T�y�&B����=�!#��D���.j�-T$�r��8��{5���!�d�e����TcC����d�A����C��1�9�FE�j�ñk��3�\!������rd%�]E��`M[� ���V�x�ϝ�W�d��[��"N�$@Vy�m�d4�8��O����~_���*���
��􎟢Y�1	���9*�N�7���=\-TjS<F���C�� �M�x)��9=�:������\�2�C/����WA9//�nB�>��t�
������}�c������I��)	H�<�G�e�
�X#�T��o������Q
!F�e�&A�b(,|��ݽv�h�L��=1���g .�靓��$�{&zLÄ��PM긒l�Nո�eEr�:+ɂ{0�s�0/1�ԟ�4?8���|�����I����f�T�_}h�I�]F��]_���D�F;�aR���SjM!E��
��D&��1�]��4^#a�y�a�V?���$�߻\��н���ֆ�猪Hׁ����;���������3�c���CM�eL��Q�^�Թ��2�R6���MK�`�ɠ��/���&Jh������D*��B_���͜g��:���L��M��q��(,������>�8t�J�h��,w]ǝZo(^���Cf��(������-��io�b��X�E�7��p�hrB��A��#c������0������B�d��`�[�X�x����Q�i@8e����ج,e��]�+��H��(���y��|���c1&������sTV-����B����m�.z��ҕ�������@@|>7P����}��jV5Q�	��a�4�T�vk�IeK�Ԭl��բB���(�����uaJ�DS�	�<2F�l5-������Q��#�ŉ%�q�Fyw�q�!QD�=_߬ǅ�`��M��O_�.v���3�^U[@b�DS[���?O�ǘ���\�
%4�s� hE��}�n��޴��'WX��.�۹���O6��>�@&Z~�o�B�s;�A?v�����&�T?&l�N߰�g/�.G7��H1��Jy%�YGV��qG˰w�Mr��A�r�KY�w�Oc|/gwE���q�39�e��(hD���oի�i�I�p��s���>Pk�ߒ�ˆd�"*�:�?dd$�7��8���Z�H��V���d��#a��W�|Џ֛���+�Y2Vv�Wy�ľD�[JB:��ᚉ�\��+.�:�V��k��G�,�}.
G"�s���ƿn�OOK���_I�ѯ͌GW�A�>̕�/P��f�/@�t�._�;Ҫu���µ�T+��~����d�ͭV�&����a�M�>�{�H��E�Џ�E@�;-��#�7��ϑ�n4�'�}db�^�_��\���zݺų��av!D���;����z������������O�Og�1!A�y*s�W����#Q�����W�&�V�9�v��"�Li���iI��hdDh��BtC�t���|�Èa��lz>
�B�Y����Z,��%�a���%�U-;�c��c���!$�ƻϗ����FRwtA�kJ0���U
�Ⴋ3H'����G�g�ۦ�f�+By���?��WI�w�*�j��J~�4GjM��ע������ �l��N�I�p�;��r'���kz?U�w�9��H�pߋ��Vrv���f(}'q0�����F*����@�Zi�BjgI�K9�ϔ��j��x?b���_�W�7�g�,�v�VrtQ��l�G�ڦ~D���B(,G��ͩ}���[��>Xve)SB�b�{�ܔ���3��䅞�e����W1'4��?�-;�����m�*�z0Ê���HX2���f�ݮ�����-Y���i�'.}ɅM�,8�C��éT=,餥��.Ҧ�4D*Vh��(�����}���X��G�c��/樴/�QS�P�ѧ�~�]_8Q��D��r�{��~�P;F��=�T��g��&���ShS��"�L4�Z��֣�zI��Ct��2]`�]�V�	��\K�4�o]�n� ޫ���Y�sjs�.z���4=h2���M�׸��ƅ^���0_
�i��ȉ���6mE�YIW<�/�ܙr�S�}��[N� o��u,1��o"����s�ϙd6dө��u�0�cȏ�P����]�JiC���	����CX:vo�nӖ?�Q9�9 ��	���P�5D��G8�{��g���~+)���D��㣕��������9��R����J����K��.f�w��Ū��~�n�Y\�+q�Rs zW(i���j�c�V�н����иe�*��ѪC�	�[�@��bo����c�?'��j�AD�#2��g�r��S#sAD�����|� �:�?��*c�sNp��2̍��#@FT/�Ӑg���ٲc�8�h˼�.sF��[���������`��~��
�&d�Чn���{�Z�����-{���̓ў���)W�l,���>�,�t�:pa�ۮ�)�f���|(T��$m�J�3I�}GO:4Qu@�!�S�1� �&Mm�rAg���{�p��𠁍��=�]�QL���J��J
�^IT~2:�A�W�֒��[���3�fJC|�(�\$3���.y;�ni�8��X�:��V��wq\#�'@eZ�$3	z�����Zt��n�Fd7��i	�Hņ�tm?e�ł��6�}���=��`�i�Z���ۿՊQdx]y1vUQ� �B�o/��T/̺� �����=��Z�'@��1���K��Z�`��z���[D��xnrM�%�n�O���#�04W�P�k����7ꬸ6f#��4��W���Zy���|$��eR��yv;���r{�3��������k�9����ai�\i	�I�G��Ӻ�(�L_^%3p�Mp��j�\���x� �T%����юFZx�:Q���B�T��x7˄�lZ����y�i�a2��dw�OQ�o Ȩ-0��K�0Hd��Ai9N�%Zp�4Mg<�˴T��>c����*�!x�,�E�?>���5�A�y��N$t�[U����&<i�H� RbD�^i��i$ e���Qd|$u�S�י^y�5�5��z�Y��'������w��{~�`>�27�|Ž?ȭ6���p�ĭ���T/}��o�T��=5iU$b���SG9��pqV��^���q��x��V�hI%`v�Lo/ zԃ�_N`õSnt�Jz���*�;��?P�����:oA�#�((�����$��"g��NR��M���,����������c��r�O��%&��m��Not �[�<@�x�E�-o�rJ�DDK&�Ք��R4�Kt�2&m�/$��ҳ9�$��	��/k����K/ލ�V<�����δ��.a�S�>6��M��>a5�e�����ħ�r�JXBA?��k�����~�q��2��EbI)3-�U�ϼ���/1���5B��=%���ܐ;d^���)�6���3]���1\��l�nŋ�ivօIw�;�cz}r�hE$�=�ӱ�̫�Gu5 ��0+j�`H_�v�HL�}Xޕ��4������Gs�N�D��GJ3�y���g��̓Rf;�>(FT�dL1�i��J�)0Ųs���j��Y��B��L��v��|��1.vj(L;�ojJpl���7�J���|܉[�?�l�^H�hhz�͂R��Iee���j�O�����Fx�i�s�D��s}���5�9ʁ��T
5��3ؠ.h�0�@.B��ڭ���sY46�������}quҧ�'���_st~����z����^s�.�-N��Zl���[��Y
�ͅN�7;9m,N���m?Z/�)��0
t��:Mֳ��P C���v��/p|
��94�-GR��É��?��YE�2�=�{e-[HI4!�A�>j�A�VK�#ľot�M�EI�)acp��6���������=.��r�,px�̰q�O6և����v��P��X�m�� ���.�`"��R�)�
٨v8��b��B.���NՁ�e�ŀÁ� ܜ�2�9YRb���Dtu�D`oF���++W��$����1e�b�Iu�E�`���_��T~�:gj�y ��-mBl��6������C¼p*f�Lv���g1N�F<�������g���I�p����>���}�DRR5�Q�����T�������1l��j�)���-�Ff�Ez��m�>d�����hZL��	����@*N�u\}5�%Kn��a�1���~�gO�BE/�~�u��7J�%�7f_���́���EˈBM5�Z���L�����^]x�G�~��mѾ�j^h��&,?���؄�`�����"R4�Q���؏����k����n<4j�
��o��]���_��?�~�D�|Q�:� kh��z�$�����z2)�C�*�s����}����=�._2��K+%�z�r�R�*�4�`�v82�X-?���bz�'��E�/�O�V��oL��F4j�=��'/5P��K��a�<hm��	�,�FV�9�Zڋ�>���2ꟼ���	Yܸ���B4M�Y �����Q�G����?��P���*��� �
�3wB�wz���B7���.d�U^��<J�����n��߯!�o���ٝ,Ɠ�%��j����нї���/�+�o �E��R�|G��LB�ɠ%!TZ�x�2�97�lqd*W���ac<���_}�.-�ゾ��&"��t�RaF��j�?�W��Q or��qbwH��/�^�Mn*Q�W���l�
endstream
endobj
18 0 obj <<
/Type /ObjStm
/N 100
/First 858
/Length 6088      
/Filter /FlateDecode
>>
stream
x��\kS���ί��舘0�_��NU aɍd�$d����	x�`�M69��t���4�1�p�Ĳ����/j��E�*^yV	^	�*�*�L%t�=���8YIV9����U�T�;UI]qa͖���q��ڊJ��[�d��Wf5�R�hYi�"�?T����V���(X��,�3h�q�d`
#*)������Ɍ�wd,��C\������⒘ݲ@�ǥX���'*�`i0��B��ZX�����?�+�`�T���t��n�P� '5@�*��+���at*�uS@�D�  8zz� *��r ��pɵ�<�������k�j�^�Y�� �>���P�Q&��S��dbV 5��q�a?a���&�uA�x����la� `�5���['qE�rL	=
���8ñ#��9j���h�0�!`�0�`T
L���h����ef&�I�{ "A��i* ��.iqPA%��D<d���X#��k�l	�P��A���L#�z�_�4 �8�q�����I�&=�p~��_����MU��7[���j�\�g��;ܪ����洙��QÛ�l<ڙ���d�``y֋�[0��`�������`���_�z-�`֗�`7�
k��
*��`̩��XGU�u��u�*�� �DU1�D'�`�<P���n��Gͼ��x�W��͏y�9�%!�0	�0��g�K��tr������oS�L(� ��Eg�� f�ϐ*X���T��|X���-S�$��������s�tK�̀ʕn�M�w�b�t���®@6��v1��A��iV2C�xX�7��gx�+S�\�P��[�a���X�I��29w�Z�J�h��:Yl���l�����pRv��n82x3L�r#�D#1����h��=8�%g�Y�+9��/��Ԡ�����e&����rS�z���[���(��(����|��1n�7�З��<�Y�I�����\+�m��yc�H��r!��+��]Rn
��`ģ���1Kg�/��U!ײ\�"�,�Z ��?�V���֟Jq(�e�p��p2��*�9.�M,���:�Fh�N�m�����ܖ��I�5�0���qz[�QzZ�}��?���%�6����kLE�e4<Ԗ��\k���Μ���&`[���n�߶�!̙0��o#�T�����E��& � *ĭ�`(g�U^���4��  G����V��2	�%g�7�9h������@M+��̩���R	f�'���cy5���Хx�p����2_Y(-mP	e�2 �jx��kx�8��!�^ql��J	�H��]�<V�=k;ũi�Xw�PV�d"a*��&�X�~�@���2������R��dG]��K^c�,�Cu�h��,#����9�|3�j�m4��h�9r!]���JK�$�m8�^�k��	��bIgij-h��!�4��o�ǑԎ2ձœ��Ȭ�0lc\gPoK�]�-�5�B��H�pD�Z�va�U��`r��0�����J�b��c��J�f�T�[�'y�zf�BQE��-G�����3��p̲C̯)hsT�t��j�L:O-؇���tա�k���G���f���?���i�]�4Α^��y�����g��֧=G�p}y��N��y�L���.��Ã�hוֈ4�{����m[�������F|�Ѥ�4�k�\$I��;�#F��>��"l��QlQ�ޱVp	�oh�.<��x�y�(��FHm�7N�V)��G�ы�f��;/\5��۞+����}ẖ�#׭b�`N41�|0c��!�L,Z�6Kї5=?�9���J\��W9��b!ԸEPI�npZE{O�밍R�#B��8B���Z��8��J���MB1��=<E1�~^�7
�P�R�	�;aeݽB�`6�!����/A��v%��=��d�����ޢ�u�k��6�$�]>)��m��X�$@�"5Y�J܍t,�\!R��=��->�9��St\�4A�!�S��\��Fb$C:�0���(d�:..���V�I
�B��L�@��b9Cv��?��F�H9��2�/M�0%�⤃T
�D<d����4��*�,���s%O�Viւ�fe�'�L+���H2T.���l�Ecv�"�a}a��c/0����:E�B�)ԝ�C5i�@��`�"�
{��e�d>2���li�(z��8�*�K�"#$�.4�>sO.�Bt�({�C)�؊Y:/�i&�pa!f��=~Y�b;���6��:��}��myk>�$V��c:��C��d��@�g,��� ������OX�󔣠2->P�����LPO8��&�{/�ŧV�p���B�'��&j���X��<��z��yԨ�0-�8`��ϣt���f�j�X�5U��/s��Sq��E�i�z�?�����!�@�m� �i<[C�2���0S#8�ϷV��I�Y"�1���g
���s����0!�y�@9\��辗*L|,��|H�Ϲ���lLPP*�꽋)3<Q�7y��}H*K��ad�A�8���7��58�� ��MD��FU�~��r* ?Ҹ��JK~y��,QӔ71 FW�5�2�(�����@�k���z$c����'[4���!��i,�lA�2��-n ��1��98�����M(��ж�p�E�U�!]R��^F��o�n�1���R��8�:�*lqH���2�󄧔�
�Ջ�O�3�
��yeJ��鹔\����8 G)�:�_M�͆R��2��`	N3���CB&�%�,�c��/P��Q�+��R�»Ӣ�qĄ'��%Ob��*0mhq�ҫ�'�JL�A��A�!G(XR"4I�.�\i����ɛf���{M߆���'�в-O��$l8.�1�t����O#�03�e�d!;JW)dQ6�1-��ʜ�
�s�����!6�����T�%G�?ܶ�g:0�D�#1�-�,�cX9���K���ze.�ܓ�������N�%����R�z�x�13�2���M��J�޾�Q�(�_������Q�?��ܸs7! �=\d�ᑔT���� �$n��ǫVPa\n�t�a��φ臎�yS����h7WA��=4^� c�Eo+�6��!����kINc�8���^����d:][���B(0�$-F.�y�3��i��,�"�8�+�!?����E��pɪ���$�E��G�#�6��� ^�R�Zņ�7��,Ώ��=Ū�-��Ny�[u�����Mѽ����R��(���B-����h�xh1[N� ����t)j��t��MW����+�#NE�Dol(�7�M��bz<E���:jnL�b�M��,x̐矻	�t3@	\�Vf���tʂ��8�P
z(�ڬ}���Ԑ�Nw���]��/Q೴���28���]sw�e �M�2Z$�j_�N/����N"�e�(��ۻ�����\ڹ��s�����=�ⲻ��.#ޅ�l���������]��xa!%�˰�'��Թ{�a��,��l��-�-S6c�)�����˚{]�!�Qy��-�t���6�|sF��^��ֻIvn�dӅ��Y�E��5�_�(��	ǻl��i8^:�h���4�+K��y;&J8=���s�U+�RԳfvz3��Oo�F��d���W�~�}�sb�}2:�U*tء�\=��በ/9�F������xe<��]�7��9��j��Ϟp|�b>��O�^�O��m�G������V}0���������W��7�1��_�����7/~?<"^�r^���p�[-{8/��r��F������^ 7χ����p
ٱb��97,��06��U+��|��6���|g|~���?�n//G�1~��Ǽ`W�������_��y���+m�C��ղ��d���ıg���q��]���Oo�8�'^�����P��xqz����N��>�O��/�i}:������oP���򶾪�������MF��z6�Է���owN>~8 ܀��d��=��sr}�7�Q=]~9���{-�������6d��g2��7�R��A�������~O��<�����n�g�����Z������͛�������S=if��$W����9��I_����\i�[P��H�{��9�����?%�k9�����o�.wO@��w`��ٽ<��xO��z�ϴdg=g�����ߐ��MW�v��Q�;r��g��cDn��f��=�o���8]����7����hBp�_���T}��63��Ys9>���/n��?�M��뻝��^"n�'[Ó�Qp[av��Ȃ�/��F�`����0�����hR�:���v����a�Gt-?}p���9j��р�6:A�W��C��!��z<�^��~�=��d��̅�,p�4��� o�7p[c@w0@9z��z��|o"�J߂�[��|����������$���.�B; ��J�c���#�ֿZ`��'�4'�)��r�ߝ�?�@ �p��'�u�N5��@��I��������{����ɻ��ӎ>-����P�M �3�Y0�͠W��_6%;k��o?F]�4t�Dv��!�w-�yv�p�ws3�'�x{l	���7�����lt~At��usy=�9k����
�.��8��������'�΀������Cp�����Q�>��f�g�a����R���N&M���v2_O~�(B�q�(����އ�'�J+l:�rEY��@i8Bځm����%jg_&�Lņ`ƫU�o���Isu
�ۏ��(n�[���N��x�M�ǧ���4����k�X<�g�4�S��r<�����
��IX߀����ov^�<y�::�?p��,��܎���.�T��_��}�
sb��C�1��Y7���.��#��r$�X?�Dn �(�Y�!��~����/o����U]�D�7礤��*2|EI���4�FR��9�hN��Ϛ������rt]�;}}��pg�x���N���*����7<�e�s��j�
V�z��g�O?��r0��r;;�^L��'����]R�*�1^���^��P$T��n\��"��1�\a�OZD�*���������B�}��P��OH#tcm���z����������aO�/[�s*�!Wz0�
�'\�`�l��������R�������wy=l�oW�p��ffƨ-Յc�f蔑'%ic.NI�~��A�^���P����~�#��R��su�x6��o6�-���g�16W�mqd��:Y������8>~�����g���$�y���a0�%F�֞�i����/�(ưp���G/�US*�q�>.����*>���1�G�:zѬ�N��6�uG�$��D�В������l|up��~��=�S�
'��QO+�s�Y��_�12S��q5)m�K��
�]�����Q1+#[���z+'�p&���BN��E�aY��[�j@�aӲ+�⚵O�_��k}C%$��ܷP��/s�����;�YC?-ѽ1-l�~�bo|3��6��lկG��~��/f�[�KMv��g����C��X�v�B�C�ti��Ƥ{bۮO�s9�!������fzAۭO�s�W�Ɵ���-r�b�����\�u����g�_�{]�\sui���9�?1�����.ힲa`����-�@ي˙.鞲�q[�>��~�C��T�e�q�Mx��.���{�&吖o�x'��%�״AS�坻�{�fMA?3��K'��%��7��t��|�R�$k;��J���vl'�Th�v_�
��y�쬓`��V+��i��iw2f]�f�f�3��~�Imu���+X狭�n�z7�!/{��r�n��v;�䊺�{���r�f�W%w��)�0:�J��e��w�;�!���}��	���*��]�[	�YH��GX�]G���[�:!3�����/��D
endstream
endobj
144 0 obj <<
/Producer (pdfTeX-1.40.14)
/Creator (TeX)
/CreationDate (D:20160315233446+09'00')
/ModDate (D:20160315233446+09'00')
/Trapped /False
/PTEX.Fullbanner (This is pdfTeX, Version 3.1415926-2.5-1.40.14 (TeX Live 2013) kpathsea version 6.1.1)
>> endobj
142 0 obj <<
/Type /ObjStm
/N 4
/First 27
/Length 146       
/Filter /FlateDecode
>>
stream
xڕ��
�0��y��A�6mW���e�7�P�A��y���5�IO_~��%Q#��	�	��m�mA^��jHc�A��󶠇��L[A���r��h�����j����N�u˙�+�$*������[G���ҧ%]���y��?�G0
endstream
endobj
145 0 obj <<
/Type /XRef
/Index [0 146]
/Size 146
/W [1 3 1]
/Root 143 0 R
/Info 144 0 R
/ID [<76F003329A35AE56A6534A0CDA1E83E2> <76F003329A35AE56A6534A0CDA1E83E2>]
/Length 406       
/Filter /FlateDecode
>>
stream
x�%�;LTA����ܫ(��<�!
�c�E޺<D�",�Y�h�PPZ@Bb��VD�$4Vt�Yh��=���9���̗L�f���_g�b0�0�<Y 	2J�'d���i2Af�C2E��OxԊ����"�ZΒ��ڷ�{F�����/�� #��I�%65}@R�y��;��$�q\{C���2���v�5xL��E�	�٢�%�;}�e6� ��!����Z�<�O��RH�H1�JJH) �H)'�:�$U��I�I�%�H0U�k���T�ꪪ��RE���&��H��S݆�\W��?�Z��?W݁��F�
��@�_X�j��Qu�ϧ�:�_���?�����SUw��T��-��*�z�sT�^'T}��?Y?��/T���a4��#l�¦��SE
endstream
endobj
startxref
341304
%%EOF
                                                  doc/AlgorithmicKinding/                                                                             0000775 0001750 0001750 00000000000 13215763513 015457  5                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               doc/AlgorithmicKinding/macros.tex                                                                   0000664 0001750 0001750 00000017274 13216017506 017473  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               % notes
\usepackage{xcolor}
\newcommand{\todo}[2]{[{\color{blue}\textbf{#1}}: {#2}]}
\newcommand{\vv}[1]{\todo{VV}{#1}}
\newcommand{\pt}[1]{\todo{PT}{#1}}

% Relations, predicates and operators
%\newcommand\PAR{|}
\newcommand\PAR{\mid}
\newcommand\DONE[1]{#1 \checkmark}
\newcommand\PDA{\mathcal{P}}
\newcommand\SEnv{\sigma} % syntactic environment
\newcommand\LEnv{\delta} % language environment
\newcommand\bisim\sim
\newcommand\Power{\wp}
\newcommand{\Silent}{\tau}
\newcommand{\Wderives}[1][{}]{\stackrel{#1}{\Longrightarrow}}
\newcommand{\LTSderives}[1][a]{\stackrel{#1}{\longrightarrow}}
\newcommand{\BPAderives}[1][a]{\stackrel{#1}{\longrightarrow}}
\newcommand{\reduces}{\rightarrow}
\newcommand{\subs}[2]{[{#1}/{#2}]}
\newcommand\Rangeof[1]{\llparenthesis#1\rrparenthesis}
\newcommand{\GFP}{\mathbf{gfp}}
\newcommand{\subj}{\mathsf{subj}}
\newcommand{\agree}{\mathsf{agree}}
\newcommand{\un}{\mathsf{un}}
\newcommand{\lin}{\mathsf{lin}}
\newcommand{\dual}[1]{\overline{#1}}
\newcommand{\dualof}{\mathsf{dual}}
\newcommand\Contr{\vdash_{c}}
\newcommand\Alg{\vdash_{a}}
%\newcommand\Contr{\vdash}
\newcommand\Guarded{\mathsf{s}} % Should this be Skips?
\newcommand\Productive{\mathsf{p}}
\newcommand\Embed[1]{{(#1)}^\dagger}
\newcommand\Unfold{\mathsf{unf}} % shortened from "unfold"
\newcommand\Unravel{\mathsf{unr}} % shortened from "unravel"
\newcommand\Norm{\mathsf{norm}}
\newcommand\NT{\mathcal{NT}} % nonterminals
%\newcommand\Tyvars{\mathcal{TV}} % not used (vv)
%\newcommand{\Labels}{\mathcal L} % The set of labels in choice types; not used (vv)
%\newcommand{\btypes}{\mathcal B} % The set of base types; not used (vv)
\newcommand{\stypes}{\mathcal S} % The set of closed session types
\newcommand{\types}{\mathcal T} % the set of (well formed) types
\newcommand{\subterms}{\mathsf{sub}}
\newcommand{\cardinality}[1]{|{#1}|}
\newcommand{\measure}{\mathcal M}
%\newcommand\TypeSim{\preceq}
\newcommand\TypeEquiv{\sim}
\newcommand{\unguarded}{\mathsf{unguarded}}
\newcommand\TL{\ensuremath{\mathcal U}} %trace language
\newcommand{\TR}{\mathsf{TR}}
\newcommand{\TRw}{\mathsf{TR}^\omega}
\newcommand\toBPATop[1]{\mathsf{BPATop} (#1)}
\newcommand\toBPA[1]{\mathsf{BPA} (#1)}
\newcommand\toCFG{\mathsf{CFG}}
\newcommand\toLHS{\mathsf{RHS}}
\newcommand\BLANK{\$}
%\newcommand\Nat{\mathbf{N}} % was: used once, never defined
\newcommand\MACHINE{\mathcal{M}}
\newcommand\RHS{\mathsf{rhs}}
\newcommand\Return{\ensuremath{\mathsf{return}}}
\newcommand\Do{\ensuremath{\mathsf{do}}}
\newcommand\Out{\ensuremath{\mathsf{out}}}
\newcommand\Fresh{\ensuremath{\mathsf{fresh}}}
\newcommand{\eqdef}{\triangleq} % equal by definition

% syntax variables
\newcommand\prekind{\upsilon}
\newcommand\kind{\kappa}
\newcommand\kinds{\stypes}
\newcommand\kindt{\types}
%\newcommand\kindsch{\Box}
\newcommand\kindsch{\mathcal C}
\newcommand{\isScheme}{~\scheme} % mathsf is always \upshape and slighter smaller
%\newcommand{\isOk}{~\mathsf{ok}}
\newcommand{\isOk}{:\mathsf{type}}
\newcommand\Unrestricted{\ensuremath{\mathbf{u}}} % \infty
\newcommand\Linear{\ensuremath{\mathbf{l}}} % 1 
%\newcommand\GEnv{\Theta}
\newcommand\GEnv{\Delta}
\newcommand\BPAprocess\Theta

% Labels for branches and choices
\newcommand\lbl{\textit}

% Keywords
\newcommand{\keyword}[1]{\mathsf{#1}}
\newcommand{\recc}{\keyword{rec}}
\newcommand{\skipk}{\keyword{skip}}
\newcommand{\intk}{\keyword{int}}
\newcommand{\unitk}{\keyword{unit}}
\newcommand{\sendk}{\keyword{send}}
\newcommand{\recvk}{\keyword{receive}}
\newcommand{\newk}{\keyword{new}}
\newcommand{\forkk}{\keyword{fork}}
% \newcommand{\inlk}{\keyword{inl}} % Are we going for labelled injection?
% \newcommand{\inrk}{\keyword{inr}}
\newcommand{\fixk}{\keyword{fix}}
\newcommand{\letk}{\keyword{let}}
\newcommand{\ink}{\keyword{in}}
\newcommand{\matchk}{\keyword{match}}
\newcommand{\withk}{\keyword{with}}
\newcommand{\selectk}{\keyword{select}}
\newcommand{\casek}{\keyword{case}}
\newcommand{\ofk}{\keyword{of}}
\newcommand{\scheme}{\keyword{sch}}
\newcommand{\End}{\keyword{end}}

% Process constructors
\newcommand{\send}[2]{\sendk\,{#1}\,{#2}}
\newcommand{\recv}[1]{\recvk\,{#1}}
\newcommand{\fork}[1]{\forkk\,{#1}}
\newcommand{\new}[1]{\nu\,{#1}}
\newcommand{\letin}[3]{\letk\,{#1}={#2}\,\ink\,{#3}}
\newcommand{\match}[2]{\matchk\,{#1}\,\withk\,[{#2}]}
\newcommand{\fix}[2]{\fixk\,{#1}.{#2}}
\newcommand{\select}[2]{\selectk{\,{#1}\,{#2}}}
\newcommand{\inject}[2]{\ink{\,{#1}\,{#2}}}
\newcommand{\case}[2]{\casek\,{#1}\,\ofk\,{#2}}

% Grammars
\newcommand{\grmeq}{\; ::= \;}
\newcommand{\grmor}{\;\mid\;}

% Theorem
\newtheorem{theorem}{Theorem}[section]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{corollary}[theorem]{Corollary}
\newtheorem{conjecture}[theorem]{Conjecture}

\theoremstyle{definition}
\newtheorem{definition}{Definition}[section]

% Listings

% Programming language: cfs
\lstdefinelanguage{cfs}{
  language=haskell,
%  style=eclipse
  basicstyle=\sffamily\small,
  extendedchars=true,
  breaklines=true,
  morekeywords={new,send,receive,skip,select,dualof,int,fork,def},
  tabsize=8,
  literate=
    {oplus}{$\oplus$}1 
    {otimes}{$\otimes$}1 
    {forall}{$\forall$}1
    {alpha}{$\alpha$}1 
    {beta}{$\beta$}1 
    {->}{$\rightarrow$}1 
    {==}{$\equiv$}1 
    {lambda}{$\lambda$}1 
    {->}{$\rightarrow$}1 
    {-o}{$\multimap$}1
}

\lstset{language=cfs}   % Default language 

%%% imported macros (Peter)

\newcommand\kw\keyword %keyword
\newcommand\MF[1]{\textup{\textit{#1}}} %metafunction
%
% general
\newcommand\Multi[1]{\overrightarrow{#1}}

% syntax types
% meta variables
\newcommand\TY{T}               %type
\newcommand\TG{G}               %ground type
\newcommand\SE{S}               %session
\newcommand\LL{l}               %label
\newcommand\GTY{G}              %ground type
\newcommand\CG{\textit{ls}}     %choice group
\newcommand\TC{\textit{tc}}     %type constructors
\newcommand\TE{\Gamma}          %type environments
\newcommand\TEempty{\cdot}
% choice group constructors
\newcommand\cgNothing\cdot
% \newcommand\cgAlt[2]{#1:#2}     %in sessions
% \newcommand\altSingle[2]{#1:#2} %in expressions
\newcommand\cgAlt[2]{#1\colon #2}     %in sessions % \colon gives
                                %better spacing
\newcommand\altSingle[2]{#1\colon #2} %in expressions
% session constructors
%\newcommand\sEnd{\kw{END}} % use \End
\newcommand\sRecv[1]{\textup{?}\,#1.}
\newcommand\sSend[1]{\textup{!}\,#1.}
\newcommand\sRecvChoice[2][{}]{\&^{#1}\{#2\}}
\newcommand\sSendChoice[2][{}]{\oplus^{#1}\{#2\}}
\newcommand\sVar{z}%{\sigma}
\newcommand\sMu[1]{\mu#1.}

\newcommand\scEnd{\kw{END}}
\newcommand\scRecvChoice[2][{}]{\&^{#1}\{#2\}}
\newcommand\scSendChoice[2][{}]{\oplus^{#1}\{#2\}}
\newcommand\scCompose[1]{#1{;}}%{#1;\,}
\newcommand\scUnit{\kw{skip}}
\newcommand\scVar{z}
\newcommand\scMu[1]{\mu#1.}


\newcommand\tNProd[2][{}]{\prod^{#1}\langle#2\rangle}
\newcommand\tNSum[2][{}]{\sum^{#1}\langle#2\rangle}


\newcommand\scRecv{\textup{?}}
\newcommand\scSend{\textup{!}}


% type constructors
\newcommand\tcUnit{\unitk}%{*}
\newcommand\tcBase{B}
\newcommand\tcPair\times
\newcommand\tcSum{+}
\newcommand\tcLolli\multimap
\newcommand\tcFun\to
\newcommand\tcPort[1]{{[#1]}}
\newcommand\tcBang{\mathop!}
% types
\newcommand\tUnit\tcUnit
\newcommand\tBase\tcBase
\newcommand\tPair[2]{#1\tcPair#2}
\newcommand\tSum[2]{#1\tcSum#2}
\newcommand\tLolli[2]{#1\tcLolli#2}
\newcommand\tFun[2]{#1\tcFun#2}
\newcommand\tPort[1]{{[#1]}}
\newcommand\tBang[1]{\tcBang#1}
\newcommand\tDyn{D}
\newcommand\tCoerce[3][{}]{#2\stackrel{#1}{\Rightarrow} #3}
\newcommand\tWild{\diamondsuit}
\newcommand\tCrash{\spadesuit}
\newcommand\tVar{z}%{X}%{\tau}
\newcommand\tMu[1]{\mu#1.}

%metafunctions
\newcommand\dom{\mathsf{dom}}
\newcommand\seq{\mathsf{seq}}
\newcommand\Free{\mathsf{free}}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
                                                                                                                                                                                                                                                                                                                                    doc/AlgorithmicKinding/main.tex                                                                     0000664 0001750 0001750 00000004560 13216017506 017125  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               \documentclass[12pt,openright,twoside]{report}

\usepackage[utf8]{inputenc}

\usepackage[portuguese,english]{babel}

\usepackage{amsmath,amssymb,amsthm}
\usepackage{stmaryrd}
\usepackage{listings,color}
\usepackage{alltt}
\usepackage{flushend}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}

\input{macros}

\begin{document}
\textbf{Algorithmic Kinding} 
\begin{figure}[ht]
\centering
  \begin{gather*}
	\frac{}{\Delta \Alg \skipk :: \kinds^\Unrestricted} \quad
	\frac{}{\Delta \Alg \,!B :: \kinds^\Linear} \quad \frac{}{\Delta
			\Alg \,?B :: \kinds^\Linear}
    \\\\
    \frac{\Delta \Alg T_1 :: k_1 \quad \Delta \Alg T_2 :: k_2 \quad k_1 \le \kindt^\Linear \quad k_2 \le \kindt^\Linear}
    	{\Delta \Alg T_1 \to T_2 :: \kindt^\Unrestricted}	
	\\\\
    \frac{\Delta \Alg T_1 :: k_1 \quad \Delta \Alg T_2 :: k_2 \quad k_1 \le \kindt^\Linear \quad k_2 \le \kindt^\Linear}
    	{\Delta \Alg T_1 \multimap T_2 :: \kindt^\Linear}	
    \\\\
    \frac{\Delta \Alg T_1 :: k_1 \quad \Delta \Alg T_2 :: k_2 \quad k_1 \le \kindt^\Linear \quad k_2 \le \kindt^\Linear}
    	{\Delta \Alg (T_1,T_2) :: \kindt^\Linear}	
    \\\\
    \frac{\Delta \Alg T_1 :: k_1 \quad...\quad \Delta \Alg T_n :: k_n \quad 
    		m=min[m_1 ... m_k], all(\le \kindt)[v_1 ... v_n]}
    	{\Delta \Alg [l_i\colon T_i]_{i\in I} :: \kindt^m}	
	\\\\
	\frac{\Delta \Alg T_1 :: k_1 \quad \Delta \Alg T_2 :: k_2 \quad all(== \kinds)[v_1,v_2]}
    	{\Delta \Alg T_1;T_2 :: \kinds^{\max(m_1, m_2)}}	    
	\\
		\frac{\Delta \Alg T_1 :: \kinds^{m_1} \quad \mbox{...} \quad \Delta \Alg T_n :: S^{m_n}}{
      \Delta \Alg \oplus\{l_i\colon T_i\}_{i\in I} :: \kinds^\Linear}
    \quad
    \frac{\Delta \Alg T_1 :: \kinds^{m_1} \quad \mbox{...} \quad \Delta \Alg T_n :: S^{m_n}}
    { \Delta \Alg \&\{l_i\colon T_i\}_{i\in I} :: \kinds^\Linear}
	\\
	 \frac{k \le T^\Linear \quad \Delta, x :: k \Alg T :: k' \quad \mbox{prekind } k' = \kindsch}
    { \Delta, x :: k \Alg \forall \alpha :: k'.T :: k}
	\quad
	\frac{\Delta, x :: k \Alg T :: k' \quad k' \le T^{\Linear}}
	    { \Delta \Alg \recc \mbox{ } x :: k.T :: k'}
	 \\	
	m_i = \mbox{multiplicity } k_i \quad v_i = \mbox{prekind } k_i \quad 1 \le i \le n 
	\end{gather*}
  \caption{Algorithmic Kinding system, $\Delta \Alg T :: \kind$}
  \label{fig:kinding}
\end{figure}
%


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:

%[\mbox{prekind } k_1 ,\mbox{ prekind } k_2]


\end{document}
                                                                                                                                                makefile                                                                                            0000664 0001750 0001750 00000000610 13217267235 012643  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               

test :
	runhaskell -isrc -itest -fforce-recomp test/Spec.hs

testKinding :
	runhaskell -isrc -itest -fforce-recomp test/Types/KindingSpec.hs

clean :
	rm `find ./ -name '*.o' -o -name '*.hi' -o -name '*.tix'` -rf

cleanOuts :
	rm test/outputs/*

cleanAll : clean cleanOuts

coverage :
	./testCoverage

backup :
	tar -cf `backup-$(date '+%Y-%m-%d-%H:%M').tar` *

.PHONY: test clean coverage
                                                                                                                        src/                                                                                                0000775 0001750 0001750 00000000000 13216002657 011727  5                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               src/Types/                                                                                          0000775 0001750 0001750 00000000000 13216750520 013032  5                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               src/Types/Kinding.hs                                                                                0000664 0001750 0001750 00000012074 13217241047 014755  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               module Types.Kinding
( isType
, kindOf
, contractive
, PreKind(..)
,Multiplicity (..)
,Kind (..)) where

--TODO review contractive, kind and Multiplicity exports (test purposes)
import Types.Types
import qualified Data.Map.Strict as Map
import Data.Either as E
import Data.List

data PreKind = Session | Arbitrary | Scheme deriving (Eq, Ord, Show)
data Multiplicity = Un | Lin deriving (Eq, Ord, Show)
data Kind = Kind PreKind Multiplicity deriving (Eq, Ord, Show)

type Env = Map.Map Id Kind
type Message = String
type KindingOut = Either Kind Message

isType :: Type -> Bool
isType t = case kinding Map.empty t of
  Left _  -> True
  Right _ -> False

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                 = False

kindOf :: Type -> Kind
kindOf t = case kinding Map.empty t of
  Left k  -> k
  Right m -> error $ "Type " ++ show t ++ " not a proper type:\n" ++ m
  -- Right _ -> error $ "Type " ++ show t ++ " not a proper type"

kinding :: Env -> Type -> KindingOut
kinding _ Skip = Left $ Kind Session Un
kinding _ (Out _) = Left $ Kind Session Lin
kinding _ (In _) = Left $ Kind Session Lin
kinding _ (Basic _) = Left $ Kind Arbitrary Un
kinding delta (Semi t u) =
  case (kinding delta t, kinding delta u) of
    (Left(Kind Session m1), Left(Kind Session m2))  ->
      Left $ Kind Session (max m1 m2)
    _                                              ->
      Right $ "One of the operands is not a session kind"
kinding delta (UnFun t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2) |  k1 <= Kind Arbitrary Lin && k2 <= Kind Arbitrary Lin          ->
      Left $ Kind Arbitrary Un
    _                                                                                         ->
      Right $ "Error Message1. Type: " ++ show (UnFun t u)
kinding delta (LinFun t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2) |  k1 <= (Kind Arbitrary Lin) && k2 <= (Kind Arbitrary Lin)          ->
      Left $ Kind Arbitrary Lin
    _                                                                                         ->
      Right $ "Error Message2. Type: " ++ show (LinFun t u)
kinding delta (Pair t u) =
  case (kinding delta t, kinding delta u) of
    (Left k1, Left k2 ) |  k1 <= (Kind Arbitrary Lin) && k2 <= (Kind Arbitrary Lin)          ->
        Left $ Kind Arbitrary Lin
    _                                                                                         ->
      Right $ "Error Message3. Type: " ++ show (Pair t u)
kinding delta (Datatype m) =
  kindingMap delta m (Kind Arbitrary Un) ("One of the components in a Datatype is a type Scheme. \nType: " ++ show m)
kinding delta (ExternalChoice m) =
  kindingMap delta m (Kind Session Lin) ("One of the components in an ExternalChoice isn't lower than a S^l. \nType: " ++ show m)
kinding delta (InternalChoice m) =
  kindingMap delta m (Kind Session Lin) ("One of the components in an InternalChoice isn't lower than a S^l. \nType: " ++ show m)
kinding delta (Rec x t) =
  let km = kinding delta t in
  case km of
    (Left k) ->
      if contractive (Map.insert x k delta) t
        then
          if (k <= (Kind Arbitrary Lin))
            then Left k
            else Right $ "The kind of the type is a type Scheme. \nType: " ++ show (Rec x t)
        else Right $ "The body of the type is not contractive. \nType: " ++ show (Rec x t)
    (Right m) -> Right m
kinding delta (Forall x t) =
  let kd = kinding delta t in
  case kd of
    (Left k) | k <= (Kind Arbitrary Lin) -> Left k
    (Right m) -> Right m
kinding delta (Var x) = -- Left $ Kind Scheme Lin --TODO: Check this
  if Map.member x delta then
    Left $ delta Map.! x
  else
    Right $ "Variable error"

  -- if (kd <= (Kind Arbitrary Lin))
  --   then Left kd
  --   else Right $ "The kind of the type is a type Scheme. \nType: " ++ show (Rec x t)

-- (Rec "a" (Var "A")) (read "rec a . A" :: Type))

kindingMap :: Env -> TypeMap -> Kind -> Message -> KindingOut
kindingMap delta m k message =
  let km = liftl $ map (kinding delta) (Map.elems m) in
  case km of
    (Left ks) ->
      if all (<= k) ks then
        Left $ maximum ks
      else
        Right message
    (Right ms) -> Right $ intercalate "\n" ms


-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType),("c",Basic CharType)]))

liftl :: [KindingOut] -> Either [Kind] [Message]
liftl xs =
  let a = rights xs in
  if length a == 0
    then
      Left $ lefts xs
    else
      Right a

-- liftElem :: KindingOut -> Either Kind Message
-- liftElem ko =
--   case ko of
--     Left (k) -> k
--     Right (m) -> m

-- Contractivity
contractive :: Env -> Type -> Bool
contractive delta (Semi t _) = contractive delta t
contractive delta (Rec _ t) = contractive delta t
contractive delta (Var x) = Map.member x delta
contractive delta (Forall _ t) = contractive delta t
contractive _ _ = True

-- (int -> int);skip -> malformed

-- These should yield Left _
-- kinding Map.empty (UnFun Skip Skip)
-- kinding Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kinding Map.empty (UnFun (In IntType) (In IntType))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    src/Types/Types.hs                                                                                  0000664 0001750 0001750 00000003757 13216224375 014512  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               {- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Types.Types
( BasicType(..)
, Type(..)
, TypeMap(..)
, Id
) where

import qualified Data.Map.Strict as Map
-- BASIC TYPES

data BasicType =
  IntType |
  CharType |
  BoolType |
  UnitType
  deriving (Eq)

-- TYPES

data Type =
  Basic BasicType |
  Skip |
  Semi Type Type |
  Out BasicType |
  In BasicType |
  UnFun Type Type |
  LinFun Type Type |
  Pair Type Type |
  ExternalChoice TypeMap |
  InternalChoice TypeMap |
  Datatype TypeMap |
  Rec String Type |
  Forall String Type |
  Var String
  deriving (Eq) -- This Eq must be redefined
  -- deriving (Eq,Show)


type Id = String

type TypeMap = Map.Map Id Type

-- TODO: Review
instance Show BasicType where
   show IntType = "Int"
   show CharType = "Char"
   show BoolType = "Bool"
   show UnitType = "()"


instance Show Type where
  show (Basic x) = show x
  show Skip = "skip"
  show (Semi x y) = "(" ++ show x ++ ";" ++ show y ++ ")"
  show (Out x) = "(" ++ "!" ++ show x ++ ")"
  show (In x) = "(" ++ "?" ++ show x ++ ")"
  show (UnFun x y) = "(" ++ show x ++ " -> " ++ show y ++ ")"
  show (LinFun x y) = "(" ++ show x ++ " -o " ++ show y ++ ")"
  show (Pair x y) = "(" ++ show x ++ " , " ++ show y ++ ")" -- Double parens ?
  show (InternalChoice x) = "+{" ++ show (Map.toList x) ++ "}"
  show (ExternalChoice x) = "&{" ++ show (Map.toList x) ++ "}"
  show (Datatype x) =   show (Map.toList x)  -- Datatype ?
  show (Rec s t) = "rec " ++ show s ++ " . " ++ show t
  show (Forall s t) = "forall " ++ show s ++ " . " ++ show t
  show (Var x) = show x


-- read "Skip -o Int -> +{a:Int,b:Bool}" :: Type
-- LinFun Skip (UnFun (Basic Int) (InternalChoice (fromList [("a",Basic Int),("b",Basic Bool)])))
                 src/Types/Parser.hs                                                                                 0000664 0001750 0001750 00000010440 13216507315 014623  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               -- https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/parsec.html
{-# LANGUAGE FlexibleContexts #-} -- binary and infix functions need this

module Types.Parser () where

import Types.Types
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import qualified Data.Map.Strict as Map

-- TODO: Skip Semi with in or Out
-- read "Skip;!Int" :: Type

-- TODO : check list
instance Read BasicType where
  readsPrec _ s = case parserBasic s of
    Right b -> [(b, "")]
    Left m -> error "basic type parse error"

instance Read Type where
  readsPrec _ s = case parserType s of
    Right t -> [(t, "")]
    Left m -> error $ "type parse error " ++ show m


-- TOKENS

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
        (haskellDef
        {
        P.reservedOpNames = [";", "!", "?", "->", "-o", "+", "&"],
        P.reservedNames = ["Int","Bool","Char", "Skip", "()", "rec", "forall"]
        })

reservedOp = P.reservedOp lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
comma      = P.comma lexer
symbol     = P.symbol lexer
whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
semi = P.semi lexer
dot = P.dot lexer
colon = P.colon lexer
-- braces = P.braces lexer
squares = P.squares lexer

rec    = reserved "rec"
forall = reserved "forall"
skip   = reserved "Skip"

-- BASIC TYPES
--   IntType | CharType | BoolType | UnitType

parserBasic :: String -> Either ParseError BasicType
parserBasic = parse parseBasicType "Context-free Sessions (Basic types)"

parseBasicType :: Parser BasicType
parseBasicType =
      (spaces >> reserved "Int"  >> spaces >> return IntType)
  <|> (spaces >> reserved "Char"  >> spaces  >> return CharType)
  <|> (spaces >> reserved "Bool"  >> spaces >> return BoolType)
  <|> (spaces >>  reserved "()" >> spaces  >> return UnitType)
  <?> "a basic type: Int, Char, Bool, or ()"

-- TYPES
-- Skip | Semi Type Type | Out BasicType | In BasicType | Basic BasicType |
-- UnFun Type Type | LinFun Type Type | Pair Type Type | ExternalChoice TypeMap |
-- InternalChoice TypeMap | Datatype TypeMap | Rec String Type | Forall String Type | Var String |

parserType :: String -> Either ParseError Type
parserType = parse parseType "Context-free Sessions (Types)"

parseType :: Parser Type
parseType =
    do{
      whiteSpace
      ; ret <- lexeme(buildExpressionParser table parseTerm)
      --; eof
      ; return ret
    } <?> "a type: skip, T;T, ..., or ..."



table = [ [binary "->" UnFun AssocRight, binary "-o" LinFun AssocRight ]
        , [binary ";" Semi AssocLeft ]
        ]

binary name fun assoc = Infix  (do{ Text.Parsec.try (symbol name); return fun }) assoc
-- prefix name fun       = Prefix (do{ reservedOp name; return fun })

-- TODO: remove
-- parseWithoutSpaces = do{spaces;a<-parseTerm;spaces; return a}

parseTerm =
  Text.Parsec.try (parens parseType)
  <|> (do {  skip ;                               return Skip })
  <|> (do { b <- parseBasicType;                  return $ Basic b })
  <|> (do { Text.Parsec.try (symbol "?"); b <- parseBasicType;      return $ In b })
  <|> (do { Text.Parsec.try (symbol "!"); b <- parseBasicType;      return $ Out b })
  <|> parens parsePair
  <|> parseExternalChoice
  <|> parseInternalChoice
  <|> squares parseDataType
  <|> parseRec
  <|> parseForall
  <|> (do { id <- identifier;                      return $ Var id })
  <?> "a type: Skip, T;T, !B, ?B, B, T->T, T-oT, (T,T), id, rec id.T, or forall id.t"

parsePair = do
  t <- parseType
  comma
  u <- parseType
  return $ Pair t u

parseRec = do
  rec
  id <- identifier
  dot
  t <- parseType
  return $ Rec id t

parseForall = do
  forall
  -- space
  id <- identifier
  dot
  t <- parseType
  return $ Forall id t

parseInternalChoice = do
  reservedOp "+"
  char '{'
  a <- sepBy1 parseBind comma
  char '}'
  return $ InternalChoice $ Map.fromList a

parseExternalChoice = do
  reservedOp "&"
  char '{'
  a <- sepBy1 parseBind comma
  char '}'
  return $ ExternalChoice $ Map.fromList a

parseDataType = do
  a <- sepBy1 parseBind comma
  return $ Datatype $ Map.fromList a

parseBind = do
  id <- identifier
  colon
  ptype <- parseType
  return (id,ptype)



--TODO: error +{leaf:Skip, node:!Int;xFormChan;xFormChan;?Int}) -> espaço no ?Int
                                                                                                                                                                                                                                test/                                                                                               0000775 0001750 0001750 00000000000 13217241543 012117  5                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               test/SpecHelper.hs                                                                                  0000664 0001750 0001750 00000000167 13216003576 014512  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               module SpecHelper
    ( module Test.Hspec
    , module Types.Parser
    ) where

import Test.Hspec
import Types.Parser
                                                                                                                                                                                                                                                                                                                                                                                                         test/Spec.hs                                                                                        0000664 0001750 0001750 00000000163 13216003576 013346  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
--{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}
                                                                                                                                                                                                                                                                                                                                                                                                             test/Types/                                                                                         0000775 0001750 0001750 00000000000 13217241507 013223  5                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               test/Types/KindingSpec.hs                                                                           0000664 0001750 0001750 00000000611 13217251025 015747  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               module Types.KindingSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Types.TestKinding
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
--import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "some legacy HUnit tests" $ do
    fromHUnitTest allTests
                                                                                                                       test/Types/ParseSpec.hs                                                                             0000664 0001750 0001750 00000000605 13217251017 015442  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               module Types.ParseSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Types.TestParse
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
--import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "some legacy HUnit tests" $ do
    fromHUnitTest allTests
                                                                                                                           test/Types/TestKinding.hs                                                                           0000664 0001750 0001750 00000021236 13217250664 016012  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               module Types.TestKinding (allTests) where

import Types.Types
import Types.Kinding
import Test.HUnit
import qualified Data.Map.Strict as Map

-- Test contractivity
-- Simple tests
test1 = TestCase (assertBool "contractive Map.empty (UnFun (Basic IntType)(Basic BoolType))" True)
test2 = TestCase (assertBool "contractive Map.empty (LinFun (Basic IntType)(Basic BoolType))" True)
test3 = TestCase (assertBool "contractive Map.empty (Pair (Basic IntType)(Basic BoolType))" True)

test4 = TestCase (assertEqual "contractive Map.empty (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
                      (contractive Map.empty (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test5 = TestCase (assertBool "contractive Map.empty (Basic IntType)" True )
test6 = TestCase (assertBool "contractive Map.empty (Out IntType)" True)

test7 = TestCase (assertEqual "for (In BoolType)" True (contractive Map.empty (In BoolType)))

test8 = TestCase (assertEqual "for (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test9 = TestCase (assertEqual "for (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test10 = TestCase (assertBool "contractive Map.empty (Skip)" True)
test11 = TestCase (assertBool "contractive Map.empty (Semi (Out IntType)(In BoolType))" True)

test12 = TestCase (assertEqual "for (Semi (Var \"x\")(In BoolType))" False
          (contractive Map.empty (Semi (Var "x")(In BoolType))))

test13 = TestCase (assertEqual "for (Rec \"x\" (In BoolType))" True
          (contractive Map.empty (Rec "x" (In BoolType))))

test14 = TestCase (assertEqual "for (Var \"x\")" False (contractive Map.empty (Var "x")))

test15 = TestCase (assertEqual "for (Var \"x\")" True (contractive (Map.fromList [("x",(Kind Session Lin))]) (Var "x")))

test16 = TestCase (assertEqual "for (Forall \"a\" (Basic BoolType))" True
          (contractive Map.empty (Forall "a" (Basic BoolType))))

test17 = TestCase (assertEqual "for (Forall \"a\" (Var \"x\"))" False
            (contractive Map.empty (Forall "a" (Var "x"))))

-- Paper examples:
test18 = TestCase (assertEqual "for (Rec \"x\" (Semi Skip (Var \"x\")))," True
            (contractive Map.empty (Rec "x" (Semi Skip (Var "x")))))

test19 = TestCase (assertEqual "for (Rec \"x\" (Semi (Var \"x\") (Out IntType)))" False
            (contractive Map.empty (Rec "x" (Semi (Var "x") (Out IntType)))))

test20 = TestCase (assertEqual "for (Rec \"x\" (Semi (Out IntType) (Var \"x\")))" True
            (contractive Map.empty (Rec "x" (Semi (Out IntType) (Var "x")))))

test21 = TestCase (assertEqual "for (Rec \"x\" (Rec \"y\" (Semi (Var \"x\") (Var \"y\"))))" False
            (contractive Map.empty (Rec "x" (Rec "y" (Semi (Var "x") (Var "y"))))))

test22 = TestCase (assertEqual "for (Rec \"x\" (Semi (Out Int) (Rec \"y\" (Semi (Var \"x\") (Var \"y\")))))" True
            (contractive Map.empty (Rec "x" (Semi (Out IntType)(Rec "y" (Semi (Var "x") (Var "y")))))))

-- Test Kinding system

test23 = TestCase (assertEqual "kindOf (UnFun (Basic IntType)(Basic BoolType))" (Kind Arbitrary Un) (kindOf (UnFun (Basic IntType)(Basic BoolType))))
test24 = TestCase (assertEqual "kindOf (LinFun (Basic IntType)(Basic BoolType))" (Kind Arbitrary Lin) (kindOf ((LinFun (Basic IntType)(Basic BoolType)))))
test25 = TestCase (assertEqual "kindOf (Pair (Basic IntType)(Basic BoolType))" (Kind Arbitrary Lin) (kindOf (Pair (Basic IntType)(Basic BoolType))))
test26 = TestCase (assertEqual "kindOf (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Arbitrary Un) (kindOf (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))
test27 = TestCase (assertEqual "kindOf (Pair (Basic IntType)(Basic BoolType))" (Kind Arbitrary Lin) (kindOf (Pair (Basic IntType)(Basic BoolType))))
test28 = TestCase (assertEqual "kindOf (Basic IntType)" (Kind Arbitrary Un) (kindOf (Basic IntType)))
test29 = TestCase (assertEqual "kindOf (Out IntType)" (Kind Session Lin) (kindOf (Out IntType)))

test30 = TestCase (assertEqual "kindOf (In BoolType)" (Kind Session Lin) (kindOf (In BoolType)))


test31 = TestCase (assertEqual "kindOf (Skip)" (Kind Session Un) (kindOf Skip))
test32 = TestCase (assertEqual "kindOf (Semi (Out IntType)(In BoolType))" (Kind Session Lin) (kindOf (Semi (Out IntType)(In BoolType))))




{--



test13 = TestCase (assertEqual "for (Rec \"x\" (In BoolType))" True
          (contractive Map.empty (Rec "x" (In BoolType))))

test14 = TestCase (assertEqual "for (Var \"x\")" False (contractive Map.empty (Var "x")))

test15 = TestCase (assertEqual "for (Var \"x\")" True (contractive (Map.fromList [("x",(Kind Session Lin))]) (Var "x")))

--- negative
test31 = TestCase (assertEqual "kindOf (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Session Lin)
          (kindOf (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test32 = TestCase (assertEqual "kindOf (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Session Lin)
          (kindOf (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test33 = TestCase (assertEqual "kindOf (Semi (Var \"x\")(In BoolType))" (Kind Session Lin) (kindOf (Semi (Var "x")(In BoolType))))

--}


validTests = "Kinding & Contractivity Unit tests" ~:TestList [
                      TestLabel "for (UnFun (Basic IntType)(Basic BoolType))"  test1,
                      TestLabel "for (LinFun (Basic IntType)(Basic BoolType))"  test2,
                      TestLabel "for (Pair (Basic IntType)(Basic BoolType))"  test3,
                      TestLabel "for (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test4,
                      TestLabel "for (Basic IntType)"  test5,
                      TestLabel "for (Out IntType)"  test6,
                      TestLabel "for (In BoolType)"  test7,
                      TestLabel "for (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test8,
                      TestLabel "for (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test9,
                      TestLabel "for (Skip)"  test10,
                      TestLabel "for (Semi (Out IntType)(In BoolType))"  test11,
                      TestLabel "for (Semi (Var \"x\")(In BoolType))"  test12,
                      TestLabel "for (Rec \"x\" (In BoolType))"  test13,
                      TestLabel "for (Var \"x\")"  test14,
                      TestLabel "for (Var \"x\")"  test15,
                      TestLabel "for (Forall \"a\" (Basic BoolType))"  test16,
                      TestLabel "for (Forall \"a\" (Var \"x\"))"  test17,
                      TestLabel "for (Rec \"x\" (Semi Skip (Var \"x\")))"  test18,
                      TestLabel "for (Rec \"x\" (Semi (Var \"x\") (Out IntType)))"  test19,
                      TestLabel "for (Rec \"x\" (Semi (Out IntType) (Var \"x\")))"  test20,
                      TestLabel "for (Rec \"x\" (Rec \"y\" (Semi (Var \"x\") (Var \"y\"))))"  test21,
                      TestLabel "for (Rec \"x\" (Semi (Out Int) (Rec \"y\" (Semi (Var \"x\") (Var \"y\")))))"  test22,
                      TestLabel "kindOf (UnFun (Basic IntType)(Basic BoolType))"  test23,
                      TestLabel "kindOf (LinFun (Basic IntType)(Basic BoolType))"  test24,
                      TestLabel "kindOf (Pair (Basic IntType)(Basic BoolType))"  test25,
                      TestLabel "kindOf (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test26,
                      TestLabel "kindOf (Pair (Basic IntType)(Basic BoolType))"  test27,
                      TestLabel "kindOf (Basic IntType)"  test28,
                      TestLabel "kindOf (Out IntType)"  test29,
                      TestLabel "kindOf (In BoolType)"  test30,
                      TestLabel "kindOf (Skip)" test31,
                      TestLabel "kindOf (Semi (Out IntType)(In BoolType))" test32

                      -- TestLabel "kindOf (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test31,
                      -- TestLabel "kindOf (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test32

                       ]

--invalidTests = TestList [TestLabel "test5" test4]
--allTests = TestList[validTests , invalidTests]
allTests = TestList[validTests]

-- shortcut to run the tests
runAll = runTestTT allTests
runValid = runTestTT validTests
--runInvalid = runTestTT invalidTests
                                                                                                                                                                                                                                                                                                                                                                  test/Types/TestParse.hs                                                                             0000664 0001750 0001750 00000021155 13217264327 015502  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               -- runTestTT allTests
-- ghc -fhpc TestParse.hs --make
-- ./TestParse
-- hpc report TestParse --exclude=Main --exclude=QC
-- hpc markup TestParse --exclude=Main --exclude=QC
module Types.TestParse(allTests) where

import Types.Types
import Types.Parser
import Test.HUnit
--TYPEMAP
import qualified Data.Map.Strict as Map

test1 = TestCase (assertEqual "for (read \"Int\")," IntType (read "Int" :: BasicType))
test2 = TestCase (assertEqual "for (read \"Char\")," CharType (read "Char" :: BasicType))
test3 = TestCase (assertEqual "for (read \"Bool\")," BoolType (read "Bool" :: BasicType))
test4 = TestCase (assertEqual "for (read \"Unit\")," UnitType (read "()" :: BasicType))

-- TESTING TYPES
-- Skip | Semi Type Type | Out BasicType | In BasicType | Basic BasicType |
-- UnFun Type Type | LinFun Type Type | Pair Type Type | ExternalChoice TypeMap |
-- InternalChoice TypeMap | Datatype TypeMap | Rec String Type | Forall String Type | Var String |

test5 = TestCase (assertEqual "for (read \"Skip\")," Skip (read "Skip" :: Type))
test6 = TestCase (assertEqual "for (read \"Int;Bool\")," (Semi (Basic IntType) (Basic BoolType)) (read "Int;Bool" :: Type))
test7 = TestCase (assertEqual "for (read \"!Int\")," (Out IntType) (read "!Int" :: Type))
test8 = TestCase (assertEqual "for (read \"?Int\")," (In IntType) (read "?Int" :: Type))
test9 = TestCase (assertEqual "for (read \"Int->Int\")," (UnFun (Basic IntType) (Basic IntType)) (read "Int->Int" :: Type))
test10 = TestCase (assertEqual "for (read \"Int-oInt\")," (LinFun (Basic IntType) (Basic IntType)) (read "Int-oInt" :: Type))
test11 = TestCase (assertEqual "for (read \"(Int,Int)\")," (Pair (Basic IntType) (Basic IntType)) (read "(Int,Int)" :: Type))

test12 = TestCase (assertEqual "for (read \"&{a:Int,b:Bool}\"),"
      (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)])) (read "&{a:Int,b:Bool}" :: Type))
test13 = TestCase (assertEqual "for (read \"+{a:Int,b:Bool}\"),"
      (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)])) (read "+{a:Int,b:Bool}" :: Type))
test14 = TestCase (assertEqual "for (read \"[a:Int,b:Bool]\"),"
      (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)])) (read "[a:Int,b:Bool]" :: Type))

test15 = TestCase (assertEqual "for (read \"rec a.Bool\")," (Rec "a" (Basic BoolType)) (read "rec a.Bool" :: Type))
test16 = TestCase (assertEqual "for (read \"forall a.Bool\")," (Forall "a" (Basic BoolType)) (read "forall a.Bool" :: Type))
test17 = TestCase (assertEqual "for (read \"z\")," (Var "z") (read "z" :: Type))

-- Precedence
test18 = TestCase (assertEqual "for (read \"(Char)\")," (Basic CharType) (read "(Char)" :: Type))
test19 = TestCase (assertEqual "for (read \"(Skip)\")," Skip (read "(Skip)" :: Type))
test20 = TestCase (assertEqual "for (read \"(?Bool)\")," (In BoolType) (read "(?Bool)" :: Type))
test21 = TestCase (assertEqual "for (read \"(!Char)\")," (Out CharType) (read "(!Char)" :: Type))
test22 = TestCase (assertEqual "for (read \"((Int,Char))\")," (Pair (Basic IntType) (Basic CharType)) (read "((Int,Char))" :: Type))

-- Test if the whitespaces are escaped
test23 = TestCase (assertEqual "for (read \" Skip\")," Skip (read " Skip " :: Type))
test24 = TestCase (assertEqual "for (read \" Int\")," IntType (read " Int " :: BasicType))
test25 = TestCase (assertEqual "for (read \"Int ; Bool\")," (Semi (Basic IntType) (Basic BoolType)) (read " Int ; Bool " :: Type))
test26 = TestCase (assertEqual "for (read \"Int -> Int\")," (UnFun (Basic IntType) (Basic IntType)) (read " Int -> Int " :: Type))
test27 = TestCase (assertEqual "for (read \"Int -o Int\")," (LinFun (Basic IntType) (Basic IntType)) (read " Int -o Int " :: Type))
test28 = TestCase (assertEqual "for (read \"( Int , Int )\")," (Pair (Basic IntType) (Basic IntType)) (read "( Int , Int )" :: Type))
test29 = TestCase (assertEqual "for (read \"rec a . A\")," (Rec "a" (Var "A")) (read "rec a . A" :: Type))

test30 = TestCase (assertEqual "for (read \"+{i : Int, b : Bool}\"),"
      (InternalChoice (Map.fromList [("i",Basic IntType),("b",Basic BoolType)])) (read "+{i : Int, b : Bool}" :: Type))

--more complex structures
test31 = TestCase (assertEqual "for (read \"((Int,Bool),a)\")," (Pair (Pair (Basic IntType)(Basic BoolType)) (Var "a")) (read "((Int,Bool),a)" :: Type))
test32 = TestCase (assertEqual "for (read \"rec a . (rec i . Int)\")," (Rec "a" (Rec "i" (Basic IntType))) (read "rec a . (rec i . Int)" :: Type))
test33 = TestCase (assertEqual "for (read \"forall a.(forall b.Bool)\")," (Forall "a" (Forall "b" (Basic BoolType))) (read "forall a.(forall b.Bool)" :: Type))

-- associativity
test34 = TestCase (assertEqual "for (read \"f -o f -> f\")," (LinFun (Var "f")(UnFun (Var "f")(Var "f"))) (read "f -o f -> f" :: Type))
test35 = TestCase (assertEqual "for (read \"(f -o f) -> f\")," (UnFun (LinFun (Var "f")(Var "f")) (Var "f")) (read "(f -o f) -> f" :: Type))
test36 = TestCase (assertEqual "for (read \"Skip;Skip;Skip\")," (Semi (Semi Skip Skip) Skip) (read "Skip;Skip;Skip" :: Type))

test37 = TestCase (assertEqual "for (read \"(Internal,Int)\")," (Pair (Var "Internal") (Basic IntType)) (read "(Internal,Int)" :: Type))
test38 = TestCase (assertEqual "for (read \"(Skiper,Int)\")," (Pair (Var "Skiper") (Basic IntType)) (read "(Skiper,Int)" :: Type))
test39 = TestCase (assertEqual "for (read \"a -> {-A comment inside-} b\")," (UnFun (Var "a") (Var "b")) (read "a -> {-A comment inside-} b" :: Type))


validTests = "test" ~: TestList [TestLabel "for (read \"Int\")" test1,
                      TestLabel "for (read \"Char\")" test2,
                      TestLabel "for (read \"Bool\")" test3,
                      TestLabel "for (read \"Unit\")" test4,
                      TestLabel "for (read \"Skip\")" test5,
                      TestLabel "for (read \"Int;Bool\")" test6,
                      TestLabel "for (read \"!Int\")" test7,
                      TestLabel "for (read \"?Int\")" test8,
                      TestLabel "for (read \"Int->Int\")" test9,
                      TestLabel "for (read \"Int-oInt\")" test10,
                      TestLabel "for (read \"(Int,Int)\")" test11,
                      TestLabel "for (read \"&{a:Int,b:Bool}\")" test12,
                      TestLabel "for (read \"+{a:Int,b:Bool}\")" test13,
                      TestLabel "for (read \"[a:Int,b:Bool]\")" test14,
                      TestLabel "for (read \"rec a.Bool\")" test15,
                      TestLabel "for (read \"forall a.Bool\")" test16,
                      TestLabel "for (read \"z\")" test17,
                      TestLabel "for (read \"(Char)\")" test18,
                      TestLabel "for (read \"(Skip)\")" test19,
                      TestLabel "for (read \"(?Bool)\")" test20,
                      TestLabel "for (read \"(!Char)\")" test21,
                      TestLabel "for (read \"((Int,Char))\")" test22,
                      TestLabel "for (read \" Skip\")" test23,
                      TestLabel "for (read \" Int\")" test24,
                      TestLabel "for (read \"Int ; Bool\")" test25,
                      TestLabel "for (read \"Int -> Int\")" test26,
                      TestLabel "for (read \"Int -o Int\")" test27,
                      TestLabel "for (read \"( Int , Int )\")" test28,
                      TestLabel "for (read \"rec a . A\")" test29,
                      TestLabel "for (read \"+{i : Int, b : Bool}\")" test30,
                      TestLabel "for (read \"((Int,Bool),a)\")" test31,
                      TestLabel "for (read \"rec a . (rec i . Int)\")" test32,
                      TestLabel "for (read \"forall a.(forall b.Bool)\")" test33,
                      TestLabel "for (read \"f -o f -> f\")" test34,
                      TestLabel "for (read \"(f -o f) -> f\")" test35,
                      TestLabel "for (read \"Skip;Skip;Skip\")" test36,
                      TestLabel "for (read \"(Internal,Int)\")" test37,
                      TestLabel "for (read \"(Skiper,Int)\")" test38,
                      TestLabel "for (read \"a -> {-A comment inside-} b\")" test39
                       -- TestLabel "test40" test40
                      ]

--invalidTests = TestList [TestLabel "test5" test4]
--allTests = TestList[validTests , invalidTests]
allTests = TestList[validTests]

-- shortcut to run the tests
runAll = runTestTT allTests
runValid = runTestTT validTests
--runInvalid = runTestTT invalidTests

-- runTestTT allTests
-- main :: IO ()
-- main = do
--     runAll
--     return ()

-- TODO: test eq and show
-- TODO: INVALID TESTS

-- Invalid whitespaces
-- testX = TestCase (assertEqual "for (read \"! Int\")," (Out IntType) (read "! Int" :: Type))
-- testX = TestCase (assertEqual "for (read \"? Int\")," (In IntType) (read "? Int" :: Type))
                                                                                                                                                                                                                                                                                                                                                                                                                   testCoverage                                                                                        0000775 0001750 0001750 00000000404 13216630232 013513  0                                                                                                    ustar   balmeida                        balmeida                                                                                                                                                                                                               #!/bin/bash
#runhaskell -isrc -itest -fforce-recomp test/Spec.hs

ghc -isrc -itest -fhpc test/Spec.hs --make
./test/Spec
mv Spec.tix test/
hpc report test/Spec --exclude=Main --exclude=QC
hpc markup test/Spec --exclude=Main --exclude=QC --destdir=test/outputs
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            