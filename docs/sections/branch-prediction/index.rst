Branch Prediction
=================

.. _front-end-bpu:
.. figure:: /figures/frontend.png
    :alt: BOOM Front-end

    The BOOM Front-end

This chapter discusses how BOOM predicts branches and then resolves
these predictions.

BOOM uses two levels of branch prediction - a fast "next-line predictor" (NLP)
and a slower but more complex "backing predictor" (BPD) [1]_. In this case,
the next-line predictor is a Branch Target Buffer and the backing predictor
is a more complicated structure like a GShare predictor.

.. toctree::
    :maxdepth: 2
    :caption: Branch Prediction:

    nl-predictor
    backing-predictor
    configurations

.. [1] Unfortunately, the terminology in the literature gets a bit
    muddled here in what to call different types and levels of branch
    predictor. I have seen “micro-BTB" versus “BTB", “NLP" versus “BHT",
    and “cache-line predictor" versus “overriding predictor". Although
    the Rocket code calls its own predictor the “BTB", I have chosen to
    refer to it in documentation as the “next-line predictor", to denote
    that it is a combinational predictor that provides single-cycle
    predictions for fetching “the next line", and the Rocket BTB
    encompasses far more complexity than just a “branch target buffer"
    structure. Likewise, I have chosen the name “backing predictor" as I
    believe it is the most accurate name, while simultaneously avoiding
    being overly descriptive of the internal design (is it a simple BHT?
    Is it tagged? Does it override the NLP?). But in short, I am open
    to better names!
