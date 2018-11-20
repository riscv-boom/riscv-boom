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
(BPD).

.. toctree::
    :maxdepth: 2
    :caption: Branch Prediction:

    Rocket-NLP-Predictor
    Backing-Predictor
    Configurations
