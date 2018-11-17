
Branch Prediction Configurations
--------------------------------

There are a number of parameters provided to govern the branch
prediction in BOOM.

### GShare Configuration Options

#### Global History Length

How long of a history should be tracked? The length of the global
history sets the size of the branch predictor. An $n$-bit history pairs
with a $2^n$ entry two-bit counter table.

### TAGE Configurations

#### Number of TAGE Tables

How many TAGE tables should be used?

#### TAGE Table Sizes

What size should each TAGE table be?

#### TAGE Table History Lengths

How long should the global history be for each table? This should be a
geometrically increasing value for each table.

#### TAGE Table Tag Sizes

What size should each tag be?

#### TAGE Table U-bit Size

How many bits should be used to describe the usefulness of an entry?


