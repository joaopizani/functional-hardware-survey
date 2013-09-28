Problem presentation
====================
With the increasing size of circuits and concerns involving power and layout, hardware
design steadily becomes a more and more complex and error-prone activity. In this scenario,
researchers have suggested and investigated (starting in the 1980s) the usage of functional
programming languages to express circuits, through the creation of Embedded Domain-Specific
Languages (EDSLs). The landscape of EDSLs for hardware description is vast, but far from
organized, comprehensive or easy to grasp. Embedded Hardware Description Languages (EHDLs) vary
in host language, simulation capabilities, verification, integration with other tools,
level of abstraction, to name a few criteria. All this variation makes choosing an EHDL a hard
and time-consuming task.

Research question
=================
In this experimentation project we compare several embedded hardware description languages,
also questioning along the way what constitutes good criteria for comparison, as well as
what circuits can be used as good case studies to evaluate the languages based on the chosen
criteria.

A further question to be investigated in this project is what recent developments in
Haskell (compiler extensions, etc.), as well as dependently-typed programming, could be used to
enhance the EHDLs studied.

Research methods to be employed
===============================
First of all, a brief review was done of secondary literature to decide which languages/libraries
to investigate. My own experience and background also helped with this choice. The chosen EHDLs
are hosted in the purely functional language Haskell and in the dependently-typed Coq. They are:
  * Lava (Chalmers and Kansas varieties) - Embedded in Haskell
  * ForSyDe - Embedded in Haskell
  * Coquet - Embedded in Coq

Using some mildly-complex circuits (yet to be defined) as case studies, the EHDLs will be compared
based on a number of criteria. Initial research in the literature that defines the EHDLs gave me
some ideas of interesting criteria. This list might be reduced if it turns out to be too big:
  * Capability/ease of simulation
  * Capability/ease of verification (of formal properties)
  * Capability to express generic (parameterized) families of circuits
    - Parameterized by functional as well as non-functional parameters
  * Integration with other tools (implementation)
    - Synthesis, theorem provers, etc.
  * Level of embedding
  * Extensibility
    - Multiple interpretations for circuits
    - User-defined datatypes

Expected outputs of this project
================================
The main output I expect from this project is a comparative analysis of the EHDLs. An interesting
side-product will be the code describing the case study circuits in the various languages. This
code shall hopefully be useful as a learning source for people interested in experimenting with
functional hardware design.

Last, but not least, one output of this project which I also find interesting is the set of
comparison criteria itself, along with the justification of why they were chosen.

