#include "thumbsim.hpp"
// These are just the register NUMBERS
#define PC_REG 15
#define LR_REG 14
#define SP_REG 13

// These are the contents of those registers
#define PC rf[PC_REG]
#define LR rf[LR_REG]
#define SP rf[SP_REG]
#define POS31MASK 0b10000000000000000000000000000000
#define POS7T0MASK 0b11111111

Stats stats;
Caches caches(0);

// CPE 315: you'll need to implement a custom sign-extension function
// in addition to the ones given below, specifically for the unconditional
// branch instruction, which has an 11-bit immediate field
unsigned int signExtend11to32ui(short i) {
  unsigned int mask = 010000000000;
  unsigned int result = 0;

  while (mask != 0) {
    if ((mask & i) != 0) {
      result += 1;
    }
    mask = mask >> 1;
    result = result << 1;
  }
  return result >> 1;
}

unsigned int signExtend16to32ui(short i) {
  return static_cast<unsigned int>(static_cast<int>(i));
}

unsigned int signExtend8to32ui(char i) {
  return static_cast<unsigned int>(static_cast<int>(i));
}

// This is the global object you'll use to store condition codes N,Z,V,C
// Set these bits appropriately in execute below.
ASPR flags;

// CPE 315: You need to implement a function to set the Negative and Zero
// flags for each instruction that does that. It only needs to take
// one parameter as input, the result of whatever operation is executing

// This function is complete, you should not have to modify it
void setCarryOverflow(int num1, int num2, OFType oftype) {
  switch (oftype) {
    case OF_ADD:
      if (((unsigned long long int)num1 + (unsigned long long int)num2) ==
          ((unsigned int)num1 + (unsigned int)num2)) {
        flags.C = 0;
      } else {
        flags.C = 1;
      }
      if (((long long int)num1 + (long long int)num2) ==
          ((int)num1 + (int)num2)) {
        flags.V = 0;
      } else {
        flags.V = 1;
      }
      break;
    case OF_SUB:
      if (num1 >= num2) {
        flags.C = 1;
      } else if (((unsigned long long int)num1 -
                  (unsigned long long int)num2) ==
                 ((unsigned int)num1 - (unsigned int)num2)) {
        flags.C = 0;
      } else {
        flags.C = 1;
      }
      if (((num1 == 0) && (num2 == 0)) ||
          (((long long int)num1 - (long long int)num2) ==
           ((int)num1 - (int)num2))) {
        flags.V = 0;
      } else {
        flags.V = 1;
      }
      break;
    case OF_SHIFT:
      // C flag unaffected for shifts by zero
      if (num2 != 0) {
        if (((unsigned long long int)num1 << (unsigned long long int)num2) ==
            ((unsigned int)num1 << (unsigned int)num2)) {
          flags.C = 0;
        } else {
          flags.C = 1;
        }
      }
      // Shift doesn't set overflow
      break;
    default:
      cerr << "Bad OverFlow Type encountered." << __LINE__ << __FILE__ << endl;
      exit(1);
  }
}

// CPE 315: You're given the code for evaluating BEQ, and you'll need to
// complete the rest of these conditions. See Page 208 of the armv7 manual
static int checkCondition(unsigned short cond) {
  switch (cond) {
    case EQ:
      if (flags.Z == 1) {
        return TRUE;
      }
      break;
    case NE:
      if (flags.Z == 0) {
        return TRUE;
      }
      break;
    case CS:
      if (flags.C == 1) {
        return TRUE;
      }
      break;
    case CC:
      if (flags.C == 0) {
        return TRUE;
      }
      break;
    case MI:
      if (flags.N == 1) {
        return TRUE;
      }
      break;
    case PL:
      if (flags.N == 0) {
        return TRUE;
      }
      break;
    case VS:
      if (flags.V == 1) {
        return TRUE;
      }
      break;
    case VC:
      if (flags.V == 0) {
        return TRUE;
      }
      break;
    case HI:
      if (flags.C == 1 && flags.Z == 0) {
        return TRUE;
      }
      break;
    case LS:
      if (flags.C == 0 || flags.Z == 1) {
        return TRUE;
      }
      break;
    case GE:
      if (flags.N == flags.V) {
        return TRUE;
      }
      break;
    case LT:
      if (flags.N != flags.V) {
        return TRUE;
      }
      break;
    case GT:
      if (flags.Z == 0 && flags.N == flags.V) {
        return TRUE;
      }
      break;
    case LE:
      if (flags.Z == 1 || flags.N != flags.V) {
        return TRUE;
      }
      break;
    case AL:
      return TRUE;
      break;
  }
  return FALSE;
}

void execute() {
  Data16 instr = imem[PC];
  Data16 instr2;
  Data32 temp(0);  // Use this for STRB instructions
  Thumb_Types itype;
  // the following counts as a read to PC
  unsigned int pctarget = PC + 2;
  unsigned int addr;
  int i, n, offset;
  unsigned int list, mask;
  int num1, num2, result, BitCount;
  unsigned int bit;

  int rmValue, rnValue, spValue, immValue, result;
  unsigned int arg1, arg2, myMask;

  /* Convert instruction to correct type */
  /* Types are described in Section A5 of the armv7 manual */
  BL_Type blupper(instr);
  ALU_Type alu(instr);
  SP_Type sp(instr);
  DP_Type dp(instr);
  LD_ST_Type ld_st(instr);
  MISC_Type misc(instr);
  COND_Type cond(instr);
  UNCOND_Type uncond(instr);
  LDM_Type ldm(instr);
  STM_Type stm(instr);
  LDRL_Type ldrl(instr);
  ADD_SP_Type addsp(instr);

  BL_Ops bl_ops;
  ALU_Ops add_ops;
  DP_Ops dp_ops;
  SP_Ops sp_ops;
  LD_ST_Ops ldst_ops;
  MISC_Ops misc_ops;

  // This counts as a write to the PC register
  rf.write(PC_REG, pctarget);

  itype = decode(ALL_Types(instr));

  // CPE 315: The bulk of your work is in the following switch statement
  // All instructions will need to have stats and cache access info added
  // as appropriate for that instruction.
  switch (itype) {
    case ALU:
      add_ops = decode(alu);
      switch (add_ops) {
        case ALU_LSLI:
          rf.write(alu.instr.lsli.rd,
                   rf[alu.instr.lsli.rm] << alu.instr.lsli.imm);

          rmValue = rf[alu.instr.lsli.rm];
          immValue = alu.instr.lsli.imm;
          result = rmValue << immValue;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rmValue > 0 && result < 0) || (rmValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;
          break;
        case ALU_ADDR:
          // needs stats and flags
          rf.write(alu.instr.addr.rd,
                   rf[alu.instr.addr.rn] + rf[alu.instr.addr.rm]);

          rnValue = rf[alu.instr.addr.rn];
          rmValue = rf[alu.instr.addr.rm];
          result = rnValue + rmValue;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rnValue > 0 && rmValue > 0 && result < 0) ||
                     (rnValue < 0 && rmValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;
          break;
        case ALU_SUBR:
          rf.write(alu.instr.subr.rd,
                   rf[alu.instr.subr.rn] - rf[alu.instr.subr.rm]);

          rnValue = rf[alu.instr.subr.rn];
          rmValue = rf[alu.instr.subr.rm];
          result = rnValue - rmValue;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rnValue > 0 && rmValue > 0 && result < 0) ||
                     (rnValue < 0 && rmValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;
          break;
        case ALU_ADD3I:
          // needs stats and flags
          rf.write(alu.instr.add3i.rd,
                   rf[alu.instr.add3i.rn] + alu.instr.add3i.imm);

          rnValue = rf[alu.instr.add3i.rn];
          immValue = alu.instr.add3i.imm;
          result = rnValue + immValue;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rnValue > 0 && immValue > 0 && result < 0) ||
                     (rnValue < 0 && immValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;
          break;
        case ALU_SUB3I:
          rf.write(alu.instr.sub3i.rd,
                   rf[alu.instr.sub3i.rn] - alu.instr.sub3i.imm);

          rnValue = rf[alu.instr.sub3i.rn];
          immValue = alu.instr.sub3i.imm;
          result = rnValue - immValue;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rnValue > 0 && immValue > 0 && result < 0) ||
                     (rnValue < 0 && immValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;
          break;
        case ALU_MOV:
          // needs stats and flags
          rf.write(alu.instr.mov.rdn, alu.instr.mov.imm);

          result = rf[alu.instr.mov.rdn];

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          break;
        case ALU_CMP:
          arg1 = rf[alu.instr.cmp.rdn];
          arg2 = alu.instr.cmp.imm;
          result = arg1 - arg2;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rnValue > 0 && immValue > 0 && result < 0) ||
                     (rnValue < 0 && immValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;
          break;
        case ALU_ADD8I:
          // needs stats and flags

          rnValue = rf[alu.instr.add8i.rdn];
          immValue = alu.instr.add8i.imm;
          result = rnValue + immValue;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rnValue > 0 && immValue > 0 && result < 0) ||
                     (rnValue < 0 && immValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;

          rf.write(alu.instr.add8i.rdn,
                   rf[alu.instr.add8i.rdn] + alu.instr.add8i.imm);
          break;
        case ALU_SUB8I:
          rnValue = rf[alu.instr.sub8i.rdn];
          immValue = alu.instr.sub8i.imm;
          result = rnValue - immValue;

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;
          flags.C = ((rnValue > 0 && immValue > 0 && result < 0) ||
                     (rnValue < 0 && immValue < 0 && result > 0))
                        ? 1
                        : 0;
          flags.V = flags.C;

          rf.write(alu.instr.sub8i.rdn,
                   rf[alu.instr.sub8i.rdn] - alu.instr.sub8i.imm);
          break;
        default:
          cout << "instruction not implemented" << endl;
          exit(1);
          break;
      }
      break;
    case BL:
      // This instruction is complete, nothing needed here
      bl_ops = decode(blupper);
      if (bl_ops == BL_UPPER) {
        // PC has already been incremented above
        instr2 = imem[PC];
        BL_Type bllower(instr2);
        if (blupper.instr.bl_upper.s) {
          addr = static_cast<unsigned int>(0xff << 24) |
                 ((~(bllower.instr.bl_lower.j1 ^ blupper.instr.bl_upper.s))
                  << 23) |
                 ((~(bllower.instr.bl_lower.j2 ^ blupper.instr.bl_upper.s))
                  << 22) |
                 ((blupper.instr.bl_upper.imm10) << 12) |
                 ((bllower.instr.bl_lower.imm11) << 1);
        } else {
          addr = ((blupper.instr.bl_upper.imm10) << 12) |
                 ((bllower.instr.bl_lower.imm11) << 1);
        }
        // return address is 4-bytes away from the start of the BL insn
        rf.write(LR_REG, PC + 2);
        // Target address is also computed from that point
        rf.write(PC_REG, PC + 2 + addr);

        stats.numRegReads += 1;
        stats.numRegWrites += 2;
      } else {
        cerr << "Bad BL format." << endl;
        exit(1);
      }
      break;
    case DP:
      dp_ops = decode(dp);
      switch (dp_ops) {
        case DP_CMP:
          // need to implement
          break;
      }
      break;
    case SPECIAL:
      sp_ops = decode(sp);
      switch (sp_ops) {
        case SP_MOV:
          // needs stats and flags
          result = rf[sp.instr.mov.rm];

          flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
          flags.Z = (result == 0) ? 1 : 0;

          rf.write((sp.instr.mov.d << 3) | sp.instr.mov.rd,
                   rf[sp.instr.mov.rm]);
          break;
        case SP_ADD:
          if (sp.instr.add.rm == SP_REG) {  // special case: ADD (SP + reg)
            rnValue = rf[(sp.instr.add.d << 3) | sp.instr.add.rd];
            spValue = SP;
            result = spValue + rnValue;

            flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
            flags.Z = (result == 0) ? 1 : 0;
            flags.C = ((rnValue > 0 && spValue > 0 && result < 0) ||
                       (rnValue < 0 && spValue < 0 && result > 0))
                          ? 1
                          : 0;
            flags.V = flags.C;

            rf.write((sp.instr.add.d << 3) | sp.instr.add.rd,
                     SP + rf[(sp.instr.add.d << 3) | sp.instr.add.rd]);
          }
          break;
        case SP_CMP:
          if (sp.instr.cmp.rm == SP_REG) {
            rnValue = rf[(sp.instr.cmp.d << 3) | sp.instr.cmp.rd];
            spValue = SP;
            result = rnValue - spValue;

            flags.N = ((result & POS31MASK) == 0) ? 0 : 1;
            flags.Z = (result == 0) ? 1 : 0;
            flags.C = ((rnValue > 0 && spValue > 0 && result < 0) ||
                       (rnValue < 0 && spValue < 0 && result > 0))
                          ? 1
                          : 0;
            flags.V = flags.C;
          }
          break;
      }
      break;

      /////////////////////////////////////
      ///// NO FLAGS FROM HERE ON OUT /////
      /////////////////////////////////////

    case LD_ST:
      // You'll want to use these load and store models
      // to implement ldrb/strb, ldm/stm and push/pop
      ldst_ops = decode(ld_st);
      switch (ldst_ops) {
        case STRI:
          // functionally complete, needs stats
          addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm * 4;
          dmem.write(addr, rf[ld_st.instr.ld_st_imm.rt]);
          break;
        case LDRI:
          // functionally complete, needs stats
          addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm * 4;
          rf.write(ld_st.instr.ld_st_imm.rt, dmem[addr]);
          break;
        case STRR:
          addr =
              rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm] * 4;
          dmem.write(addr, rf[ld_st.instr.ld_st_reg.rt]);
          break;
        case LDRR:
          addr =
              rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm] * 4;
          rf.write(ld_st.instr.ld_st_reg.rt, dmem[addr]);
          break;
        case STRBI:
          // need to implement
          addr = ld_st.instr.ld_st_imm.imm;
          dmem.write(addr, rf[ld_st.instr.ld_st_imm.rt] & POS7T0MASK);
          break;
        case LDRBI:
          // need to implement
          addr = ld_st.instr.ld_st_imm.imm;
          rf.write(ld_st.instr.ld_st_imm.rt, dmem[addr] & POS7T0MASK);
          break;
        case STRBR:
          // need to implement
          addr = rf[ld_st.instr.ld_st_reg.rn];
          dmem.write(addr, rf[ld_st.instr.ld_st_reg.rt] & POS7T0MASK);
          break;
        case LDRBR:
          // need to implement
          addr = rf[ld_st.instr.ld_st_reg.rn];
          rf.write(ld_st.instr.ld_st_reg.rt, dmem[addr] & POS7T0MASK);
          break;
      }
      break;
    case MISC:
      misc_ops = decode(misc);
      switch (misc_ops) {
        case MISC_PUSH:
          // need to implement
          mask = 000000001;
          for (i = 0; i < 8; i++) {
            if ((mask & misc.instr.push.reg_list) != 0) {
              rf.write(SP_REG, rf[SP_REG] - 4);
              dmem.write(rf[SP_REG], rf[i]);
            }
            mask = mask << 1;
          }
          break;
        case MISC_POP:
          // need to implement
          mask = 000000001;
          for (i = 0; i < 8; i++) {
            if ((mask & misc.instr.push.reg_list) != 0) {
              rf.write(i, dmem[rf[SP_REG]]);
              rf.write(SP_REG, rf[SP_REG] + 4);
            }
            mask = mask << 1;
          }
          break;
        case MISC_SUB:
          // functionally complete, needs stats
          rf.write(SP_REG, SP - (misc.instr.sub.imm * 4));
          break;
        case MISC_ADD:
          // functionally complete, needs stats
          rf.write(SP_REG, SP + (misc.instr.add.imm * 4));
          break;
      }
      break;
    case COND:
      decode(cond);
      // Once you've completed the checkCondition function,
      // this should work for all your conditional branches.
      // needs stats
      if (checkCondition(cond.instr.b.cond)) {
        rf.write(PC_REG, PC + 2 * signExtend8to32ui(cond.instr.b.imm) + 2);
      }
      break;
    case UNCOND:
      // Essentially the same as the conditional branches, but with no
      // condition check, and an 11-bit immediate field
      decode(uncond);
      rf.write(PC_REG, PC + 2 * signExtend11to32ui(uncond.instr.b.imm) + 2);
      break;
    case LDM:
      decode(ldm);

      // need to implement
      mask = 010000000;

      int ldmCnt = 0;
      for (i = 7; i >= 0; i--) {
        if ((mask & ldm.instr.ldm.reg_list) != 0) {
          rf.write(i, dmem[rf[ldm.instr.ldm.rn] + (ldmCnt * 4)]);
          ldmCnt += 1;
        }
        mask = mask >> 1;
      }

      break;
    case STM:
      decode(stm);
      // need to implement
      mask = 000000001;

      int stmCnt = 0;
      for (i = 0; i < 8; i++) {
        if ((mask & stm.instr.stm.reg_list) != 0) {
          dmem.write(rf[stm.instr.stm.rn] + (stmCnt * 4), rf[i]);
          stmCnt += 1;
        }
        mask = mask << 1;
      }
      break;
    case LDRL:
      // This instruction is complete, nothing needed
      decode(ldrl);
      // Need to check for alignment by 4
      if (PC & 2) {
        addr = PC + 2 + (ldrl.instr.ldrl.imm) * 4;
      } else {
        addr = PC + (ldrl.instr.ldrl.imm) * 4;
      }
      // Requires two consecutive imem locations pieced together
      temp = imem[addr] | (imem[addr + 2] << 16);  // temp is a Data32
      rf.write(ldrl.instr.ldrl.rt, temp);

      // One write for updated reg
      stats.numRegWrites++;
      // One read of the PC
      stats.numRegReads++;
      // One mem read, even though it's imem, and there's two of them
      stats.numMemReads++;
      break;
    case ADD_SP:
      // needs stats
      decode(addsp);
      rf.write(addsp.instr.add.rd, SP + (addsp.instr.add.imm * 4));
      break;
    default:
      cout << "[ERROR] Unknown Instruction to be executed" << endl;
      exit(1);
      break;
  }
}
