//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package object boom extends
   boom.constants.BOOMDebugConstants with
   boom.constants.BrPredConstants with
   boom.constants.ScalarOpConstants with
   boom.constants.ExcCauseConstants with
   boom.constants.RISCVConstants with
   boom.constants.IQType
{
   val START_ADDR = 0x100
}
