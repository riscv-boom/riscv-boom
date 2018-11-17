Branch Prediction
=================

This chapter discusses how BOOM predicts branches and then resolves
these predictions.

BOOM uses two levels of branch prediction- a single-cycle “next-line
predictor" (NLP) and a slower but more complex “backing predictor"
(BPD).[^1]

.. toctree::
    :maxdepth: 2
    :caption: Branch Prediction:

    Rocket-NLP-Predictor
    Backing-Predictor
    Configurations
