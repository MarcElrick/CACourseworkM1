-- Sigma16 M1: ALU.hs
-- John T. O'Donnell, 2021
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

-- Control Algorithm and control circuit

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Circuit.Control where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import Circuit.Interface

-- This is the high level control algorithm, written using
-- assignment statements to describe the effect that will take
-- place at the end of a clock cycle.  Each statement is decorated
-- with the list of control signals that must be asserted during
-- the clock cycle to make the datapath perform the operation.
-- Some of the Sigma16 instructions are unimplemented, but the
-- key ones are all defined.

{-
repeat forever
  st_instr_fet:
    ir := mem[pc], pc++;
       {ctl_ma_pc, ctl_ir_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}
  st_dispatch:
  case ir_op of

    0 -> -- add instruction
        st_add:  reg[ir_d] := reg[ir_sa] + reg[ir_sb]
          assert [ctl_alu_abc=000, ctl_rf_alu, ctl_rf_ld, ctl_rf_ldcc]

    1 -> -- sub instruction
        st_sub:  reg[ir_d] := reg[ir_sa] - reg[ir_sb]
          assert [ctl_alu_abc=001, ctl_rf_alu, ctl_rf_ld, ctl_rf_ldcc]

    2 -> -- mul instruction
        st_mul0: reg[ir_d] := reg[ir_sa] * reg[ir_sb]
          assert [ctl_mult, ctl_rf_ld]
        st_mul1: wait until multiply.ready is true
          assert [ctl_mult]
        st_mul2:

    3 -> -- div instruction
        -- unimplemented, does nothing

    4 -> -- cmp instruction
        st_cmp:  reg[15] := alu_cmp (reg[ir_sa], reg[ir_sb])
           assert [ctl_alu_abc=100, ctl_rf_ldcc]

    11 -> -- trap instruction
        st_trap0:
          -- The simulation driver detects that a trap has been
          -- executed by observing when the control algorithm
          -- enters this state, and it performs the system action
          -- requested by the trap operand in Rd.

    15 -> -- expand to RX format
        -- This code allows expansion to a two-word format with
        -- an address operand.  The instruction is determined by a
        -- secondary opcode in the b field of the ir

      case ir_sb of

        0 -> -- lea instruction
            st_lea0:  ad := mem[pc], pc++
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_lea1:  ad := reg[ir_sa] + ad
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu,
                      ctl_rf_ld, ctl_rf_alu]
            st_lea2:

        1 -> -- load instruction
            st_load0:  ad := mem[pc], pc++
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_load1:  ad := reg[ir_sa] + ad
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu]
            st_load2:  reg[ir_d] := mem[ad]
                assert [ctl_rf_ld]

        2 -> -- store instruction
            st_store0:  ad := mem[pc], pc++
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_store1:  ad := reg[ir_sa] + ad
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu]
            st_store2:
              mem[addr] := reg[ir_d]
                assert [ctl_rf_sd, ctl_sto]

        3 -> --  jump instruction
            st_jump0:  ad := mem[pc], pc++
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_jump1:  ad := reg[ir_sa] + ad
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu]
            st_jump2:  pc := ad
              assert [ctl_pc_ad, ctl_pc_ld]

        4 -> -- jumpc0 instruction
            st_jumpc00:  ad := mem[pc], pc++
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_jumpc01:  ad := reg[ir_sa] + ad
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu]
            st_jumpc02:  if inv condcc then pc := ad
              assert [ctl_pc_ad, if inv condcc then ctl_pc_ld]

        5 -> -- jumpc1 instruction
            st_jumpc10:  ad := mem[pc], pc++
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_jumpc11:  ad := reg[ir_sa] + ad
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu]
            st_jumpc12: if condcc then pc := ad
              assert [ctl_pc_ad, if condcc then ctl_pc_ld]

        6 -> -- jal instruction
            st_jal0:  ad := mem[pc], pc++
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_jal1:  ad := reg[ir_sa] + ad
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu]
            st_jal2: reg[ir_d] := pc, pc := ad,
              assert [ctl_rf_ld, ctl_rf_pc, ctl_pc_ld, ctl_pc_ad]


        7 -> loadxi instruction
            st_loadxi0: ad := mem[pc], pc++;
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abc=011, ctl_pc_ld]
            st_loadxi1:  ad := reg[ir_sa] + ad;
              assert [ctl_y_ad, ctl_alu_abc=000, ctl_ad_ld, ctl_ad_alu]
            st_loadxi2:  reg[ir_d] := mem[ad]
                assert [ctl_rf_ld] -- Operates just like load up to now.
            st_loadxi3: reg[ir_sa] := reg[ir_sa] + 1 -- increment counter by 1
                assert [ctl_alu_abc=011, ctl_rf_ld, ctl_rf_alu, ctl_rf_ldxi]


-- The remaining opcodes are used in the full Sigma16 architecture,
-- but in the Core they are unimplemented and treated as nop

        8 -> -- nop
        9 -> -- nop
        10 -> -- nop
        11 -> -- nop
        12 -> -- nop
        13 -> -- nop
        14 -> -- nop
        15 -> -- nop
-}

----------------------------------------------------------------------
--			   Control circuit
----------------------------------------------------------------------

control
  :: CBit a
  => a         -- reset
  -> [a]       -- ir
  -> [a]       -- cc
  -> a         -- ready signal from multiplier
  -> SysIO a   -- I/O
  -> (CtlState a, a, CtlSig a)

control reset ir cc ready (SysIO {..}) = (ctlstate,start,ctlsigs)
  where

-- Fields of instruction and conditional control
      ir_op = field ir  0 4       -- instruction opcode
      ir_d  = field ir  4 4       -- instruction destination register
      ir_sa = field ir  8 4       -- instruction source a register
      ir_sb = field ir 12 4       -- instruction source b register
      condcc = indexbit ir_d cc

-- Control mode is either io_DMA or cpu
      cpu = inv io_DMA

-- Control states
      start = orw
        [reset,
         st_add, st_sub, st_mul2, st_cmp, st_trap0,
         st_lea2,  st_load2, st_store2, st_jump2,
         st_jumpc02, st_jumpc12, st_jal2, st_loadxi3] -- loadxi and mul states added
      st_start = and2 start cpu

      dff_instr_fet = dff (or2 st_start (and2 dff_instr_fet io_DMA))
      st_instr_fet  = and2 dff_instr_fet cpu

      dff_dispatch = dff (or2 st_instr_fet (and2 dff_dispatch io_DMA))
      st_dispatch  = and2 dff_dispatch cpu
      pRRR = demux4w ir_op st_dispatch
      pXX  = demux4w ir_sb (pRRR!!14)
      pRX  = demux4w ir_sb (pRRR!!15)

-- lea control states
      dff_lea0 = dff (or2 (pRX!!0) (and2 dff_lea0 io_DMA))
      st_lea0  = and2 dff_lea0 cpu
      dff_lea1 = dff (or2 st_lea0 (and2 dff_lea1 io_DMA))
      st_lea1  = and2 cpu dff_lea1
      dff_lea2 = dff (or2 st_lea1 (and2 dff_lea2 io_DMA))
      st_lea2  = and2 dff_lea2 cpu

-- load control states
      dff_load0 = dff (or2 (pRX!!1) (and2 dff_load0 io_DMA))
      st_load0  = and2 dff_load0 cpu
      dff_load1 = dff (or2 st_load0 (and2 dff_load1 io_DMA))
      st_load1  = and2 cpu dff_load1
      dff_load2 = dff (or2 st_load1 (and2 dff_load2 io_DMA))
      st_load2  = and2 cpu dff_load2

-- store control states
      dff_store0 = dff (or2 (pRX!!2) (and2 dff_store0 io_DMA))
      st_store0  = and2 cpu dff_store0
      dff_store1 = dff (or2 st_store0 (and2 dff_store1 io_DMA))
      st_store1  = and2 dff_store1 cpu
      dff_store2 = dff (or2 st_store1 (and2 dff_store2 io_DMA))
      st_store2  = and2 dff_store2 cpu

-- jump control states
      dff_jump0 = dff (or2 (pRX!!3) (and2 dff_jump0 io_DMA))
      st_jump0  = and2 dff_jump0 cpu
      dff_jump1 = dff (or2 st_jump0 (and2 dff_jump1 io_DMA))
      st_jump1  = and2 dff_jump1 cpu
      dff_jump2 = dff (or2 st_jump1 (and2 dff_jump2 io_DMA))
      st_jump2  = and2 cpu dff_jump2

-- jumpc0 control states
      dff_jumpc00 = dff (or2 (pRX!!4) (and2 dff_jumpc00 io_DMA))
      st_jumpc00  = and2 dff_jumpc00 cpu
      dff_jumpc01 = dff (or2 st_jumpc00 (and2 dff_jumpc01 io_DMA))
      st_jumpc01  = and2 dff_jumpc01 cpu
      dff_jumpc02 = dff (or2 st_jumpc01 (and2 dff_jumpc02 io_DMA))
      st_jumpc02  = and2 dff_jumpc02 cpu

-- jumpc1 control states
      dff_jumpc10 = dff (or2 (pRX!!5) (and2 dff_jumpc10 io_DMA))
      st_jumpc10  = and2 dff_jumpc10 cpu
      dff_jumpc11 = dff (or2 st_jumpc10 (and2 dff_jumpc11 io_DMA))
      st_jumpc11  = and2 dff_jumpc11 cpu
      dff_jumpc12 = dff (or2 st_jumpc11 (and2 dff_jumpc12 io_DMA))
      st_jumpc12  = and2 dff_jumpc12 cpu

-- jal control states
      dff_jal0 = dff (or2 (pRX!!6) (and2 dff_jal0 io_DMA))
      st_jal0  = and2 dff_jal0 cpu
      dff_jal1 = dff (or2 st_jal0 (and2 dff_jal1 io_DMA))
      st_jal1  = and2 dff_jal1 cpu
      dff_jal2 = dff (or2 st_jal1 (and2 dff_jal2 io_DMA))
      st_jal2  = and2 dff_jal2 cpu

-- loadxi control states
      dff_loadxi0 = dff (or2 (pRX!!7) (and2 dff_loadxi0 io_DMA))
      st_loadxi0  = and2 dff_loadxi0 cpu
      dff_loadxi1 = dff (or2 st_loadxi0 (and2 dff_loadxi1 io_DMA))
      st_loadxi1  = and2 cpu dff_loadxi1
      dff_loadxi2 = dff (or2 st_loadxi1 (and2 dff_loadxi2 io_DMA))
      st_loadxi2  = and2 cpu dff_loadxi2
      dff_loadxi3 = dff (or2 st_loadxi2 (and2 dff_loadxi3 io_DMA))
      st_loadxi3  = and2 cpu dff_loadxi3


-- RRR control states
      dff_add   = dff (or2 (pRRR!!0) (and2 dff_add io_DMA))
      st_add    = and2 dff_add cpu

      dff_sub   = dff (or2 (pRRR!!1) (and2 dff_sub io_DMA))
      st_sub    = and2 dff_sub cpu

      dff_mul0  = dff (or2 (pRRR!!2) (and2 dff_mul0 io_DMA))  
      -- New states for mul
      st_mul0   = and2 dff_mul0 cpu
      dff_mul1  = dff (or2 st_mul0 (and2 dff_mul1 io_DMA))
      st_mul1   = and2 dff_mul1 cpu
      --Only progress to mul2 when multiply completed
      dff_mul2  = dff (or2 (and2 ready st_mul1) (and2 dff_mul2 io_DMA))
      st_mul2   = and2 dff_mul2 cpu

      dff_div0  = dff (or2 (pRRR!!3) (and2 dff_div0 io_DMA))
      st_div0   = and2 dff_div0 cpu

      dff_cmp   = dff (or2 (pRRR!!4) (and2 dff_cmp io_DMA))
      st_cmp    = and2 dff_cmp cpu

      dff_trap0 = dff (or2 (pRRR!!11) (and2 dff_trap0 io_DMA))
      st_trap0  = and2 dff_trap0 cpu

-- Generate control signals
-- New states added to match assertions
      ctl_rf_ld   = orw [st_load2,st_lea1,st_add,st_sub,
                           st_jal2, st_loadxi2, st_loadxi3]
      ctl_rf_ldcc = orw [st_cmp, st_add, st_sub]
      ctl_rf_pc   = orw [st_jal2]
      ctl_rf_alu  = orw [st_lea1,st_add,st_sub, st_loadxi3]
      ctl_rf_sd   = orw [st_store2,st_jumpc00]
      ctl_rf_ldxi = orw [st_loadxi3]
      ctl_mult    = orw [st_mul0, st_mul1]
      ctl_alu_a   = orw [st_cmp]
      ctl_alu_b   = orw [st_instr_fet,st_load0,st_store0,st_lea0,
                         st_jump0, st_jumpc00, st_jumpc10, st_jal0, st_loadxi0, st_loadxi3]
      ctl_alu_c   = orw [st_instr_fet,st_load0,st_store0,st_lea0,
                         st_jump0, st_jumpc00, st_jumpc10,
                         st_sub,st_jumpc00,st_jal0, st_loadxi0, st_loadxi3]
      ctl_ir_ld   = orw [st_instr_fet]
      ctl_pc_ld   = orw [st_instr_fet, st_lea0, st_load0, st_store0,
                           st_jump0, st_jump2,
                           st_jumpc00, and2 (inv condcc) st_jumpc02,
                           st_jumpc10, and2 condcc st_jumpc12,
                           st_jal0, st_jal2, st_loadxi0]
      ctl_pc_ad   = orw [st_jump2, st_jumpc02, st_jumpc12, st_jal2]
      ctl_ad_ld   = orw [st_load0,st_load1,st_lea0,st_lea1,st_store0,
                         st_store1,st_jumpc00,st_jumpc01,
                         st_jumpc10,st_jumpc11,st_jump0,st_jump1,
                         st_jal0,st_jal1,st_loadxi0, st_loadxi1]
      ctl_ad_alu  = orw [st_load1,st_store1,st_jump1,st_jumpc01,st_jumpc11,st_jal1, st_loadxi1]
      ctl_ma_pc   = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpc10,st_jumpc00,st_jump0,st_jal0, st_loadxi0]
      ctl_x_pc    = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpc10,st_jumpc00,st_jump0,st_jal0, st_loadxi0]
      ctl_y_ad    = orw [st_load1,st_store1,st_lea1,st_jumpc11,
                         st_jumpc01,st_jump1,st_jal1,st_loadxi1]
      ctl_sto     = orw [st_store2]

-- Record of control states and signals
      ctlstate = CtlState {..}
      ctlsigs = CtlSig {..}

indexbit :: Bit a => [a] -> [a] -> a
indexbit [] [x] = x
indexbit (c:cs) xs =
  mux1 c (indexbit cs (drop i xs))
         (indexbit cs (take i xs))
  where i = 2 ^ length cs
