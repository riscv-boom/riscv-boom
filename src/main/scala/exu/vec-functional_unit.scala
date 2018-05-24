//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Functional Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Hankun Zhao, Christopher Celio
// 2013 Mar 10
//
// If regfile bypassing is disabled, then the functional unit must do its own
// bypassing in here on the WB stage (i.e., bypassing the io.resp.data)

package boom.exu
import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.util._
import freechips.rocketchip.tile
import chisel3.experimental.chiselName
import boom.bpu.{BpredType, BranchPredInfo, BTBsaUpdate}
import boom.common._
import boom.ifu._
import boom.util._

@chiselName
class VALUUnit(num_stages: Int = 1) (implicit p: Parameters)
      extends PipelinedFunctionalUnit(num_stages = num_stages,
         num_bypass_stages = num_stages,
         earliest_bypass_stage = 0,
         data_width = 128,
         is_branch_unit = false)(p)
{

}
