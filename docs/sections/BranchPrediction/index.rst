Branch Prediction
=================

.. _fetch-unit-bpu:
.. figure:: /figures/frontend.png
    :alt: Fetch Unit

    The Fetch Unit

This chapter discusses how BOOM predicts branches and then resolves
these predictions.

BOOM uses two levels of branch prediction- a single-cycle “next-line
predictor" (NLP) and a slower but more complex “backing predictor"
(BPD) [1]_.

.. toctree::
    :maxdepth: 2
    :caption: Branch Prediction:

    Rocket-NLP-Predictor
    Backing-Predictor
    Configurations

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


