import "DPI-C" function void tma_counter_store(input int tile_id, input int idx, input longint unsigned value);
import "DPI-C" function void tma_counter_dump_final(input int tile_id);

module SimTMACounterDump #(
    parameter NUM_COUNTERS = 110,
    parameter TILE_ID = 0
) (
    input        clock,
    input        reset,
    input [63:0] counters_0,
    input [63:0] counters_1,
    input [63:0] counters_2,
    input [63:0] counters_3,
    input [63:0] counters_4,
    input [63:0] counters_5,
    input [63:0] counters_6,
    input [63:0] counters_7,
    input [63:0] counters_8,
    input [63:0] counters_9,
    input [63:0] counters_10,
    input [63:0] counters_11,
    input [63:0] counters_12,
    input [63:0] counters_13,
    input [63:0] counters_14,
    input [63:0] counters_15,
    input [63:0] counters_16,
    input [63:0] counters_17,
    input [63:0] counters_18,
    input [63:0] counters_19,
    input [63:0] counters_20,
    input [63:0] counters_21,
    input [63:0] counters_22,
    input [63:0] counters_23,
    input [63:0] counters_24,
    input [63:0] counters_25,
    input [63:0] counters_26,
    input [63:0] counters_27,
    input [63:0] counters_28,
    input [63:0] counters_29,
    input [63:0] counters_30,
    input [63:0] counters_31,
    input [63:0] counters_32,
    input [63:0] counters_33,
    input [63:0] counters_34,
    input [63:0] counters_35,
    input [63:0] counters_36,
    input [63:0] counters_37,
    input [63:0] counters_38,
    input [63:0] counters_39,
    // New core counters (40-59)
    input [63:0] counters_40,
    input [63:0] counters_41,
    input [63:0] counters_42,
    input [63:0] counters_43,
    input [63:0] counters_44,
    input [63:0] counters_45,
    input [63:0] counters_46,
    input [63:0] counters_47,
    input [63:0] counters_48,
    input [63:0] counters_49,
    input [63:0] counters_50,
    input [63:0] counters_51,
    input [63:0] counters_52,
    input [63:0] counters_53,
    input [63:0] counters_54,
    input [63:0] counters_55,
    input [63:0] counters_56,
    input [63:0] counters_57,
    input [63:0] counters_58,
    input [63:0] counters_59,
    // Memory ordering counters (60-67)
    input [63:0] counters_60,
    input [63:0] counters_61,
    input [63:0] counters_62,
    input [63:0] counters_63,
    input [63:0] counters_64,
    input [63:0] counters_65,
    input [63:0] counters_66,
    input [63:0] counters_67,
    // Data dependency counters (68-74)
    input [63:0] counters_68,
    input [63:0] counters_69,
    input [63:0] counters_70,
    input [63:0] counters_71,
    input [63:0] counters_72,
    input [63:0] counters_73,
    input [63:0] counters_74,
    // L2 cache counters (75-91)
    input [63:0] counters_75,
    input [63:0] counters_76,
    input [63:0] counters_77,
    input [63:0] counters_78,
    input [63:0] counters_79,
    input [63:0] counters_80,
    input [63:0] counters_81,
    input [63:0] counters_82,
    input [63:0] counters_83,
    input [63:0] counters_84,
    input [63:0] counters_85,
    input [63:0] counters_86,
    input [63:0] counters_87,
    input [63:0] counters_88,
    input [63:0] counters_89,
    input [63:0] counters_90,
    input [63:0] counters_91,
    // OOO engine counters (92-98)
    input [63:0] counters_92,
    input [63:0] counters_93,
    input [63:0] counters_94,
    input [63:0] counters_95,
    input [63:0] counters_96,
    input [63:0] counters_97,
    input [63:0] counters_98,
    // Fetch/decode counters (99)
    input [63:0] counters_99,
    // L3 TMA counters (100-108)
    input [63:0] counters_100,
    input [63:0] counters_101,
    input [63:0] counters_102,
    input [63:0] counters_103,
    input [63:0] counters_104,
    input [63:0] counters_105,
    input [63:0] counters_106,
    input [63:0] counters_107,
    input [63:0] counters_108,
    // L2 extra counter
    input [63:0] counters_109
);

    reg enabled;
    wire [63:0] ctr_array [0:109];

    assign ctr_array[0]  = counters_0;
    assign ctr_array[1]  = counters_1;
    assign ctr_array[2]  = counters_2;
    assign ctr_array[3]  = counters_3;
    assign ctr_array[4]  = counters_4;
    assign ctr_array[5]  = counters_5;
    assign ctr_array[6]  = counters_6;
    assign ctr_array[7]  = counters_7;
    assign ctr_array[8]  = counters_8;
    assign ctr_array[9]  = counters_9;
    assign ctr_array[10] = counters_10;
    assign ctr_array[11] = counters_11;
    assign ctr_array[12] = counters_12;
    assign ctr_array[13] = counters_13;
    assign ctr_array[14] = counters_14;
    assign ctr_array[15] = counters_15;
    assign ctr_array[16] = counters_16;
    assign ctr_array[17] = counters_17;
    assign ctr_array[18] = counters_18;
    assign ctr_array[19] = counters_19;
    assign ctr_array[20] = counters_20;
    assign ctr_array[21] = counters_21;
    assign ctr_array[22] = counters_22;
    assign ctr_array[23] = counters_23;
    assign ctr_array[24] = counters_24;
    assign ctr_array[25] = counters_25;
    assign ctr_array[26] = counters_26;
    assign ctr_array[27] = counters_27;
    assign ctr_array[28] = counters_28;
    assign ctr_array[29] = counters_29;
    assign ctr_array[30] = counters_30;
    assign ctr_array[31] = counters_31;
    assign ctr_array[32] = counters_32;
    assign ctr_array[33] = counters_33;
    assign ctr_array[34] = counters_34;
    assign ctr_array[35] = counters_35;
    assign ctr_array[36] = counters_36;
    assign ctr_array[37] = counters_37;
    assign ctr_array[38] = counters_38;
    assign ctr_array[39] = counters_39;
    // New core counters
    assign ctr_array[40] = counters_40;
    assign ctr_array[41] = counters_41;
    assign ctr_array[42] = counters_42;
    assign ctr_array[43] = counters_43;
    assign ctr_array[44] = counters_44;
    assign ctr_array[45] = counters_45;
    assign ctr_array[46] = counters_46;
    assign ctr_array[47] = counters_47;
    assign ctr_array[48] = counters_48;
    assign ctr_array[49] = counters_49;
    assign ctr_array[50] = counters_50;
    assign ctr_array[51] = counters_51;
    assign ctr_array[52] = counters_52;
    assign ctr_array[53] = counters_53;
    assign ctr_array[54] = counters_54;
    assign ctr_array[55] = counters_55;
    assign ctr_array[56] = counters_56;
    assign ctr_array[57] = counters_57;
    assign ctr_array[58] = counters_58;
    assign ctr_array[59] = counters_59;
    // Memory ordering counters
    assign ctr_array[60] = counters_60;
    assign ctr_array[61] = counters_61;
    assign ctr_array[62] = counters_62;
    assign ctr_array[63] = counters_63;
    assign ctr_array[64] = counters_64;
    assign ctr_array[65] = counters_65;
    assign ctr_array[66] = counters_66;
    assign ctr_array[67] = counters_67;
    // Data dependency counters
    assign ctr_array[68] = counters_68;
    assign ctr_array[69] = counters_69;
    assign ctr_array[70] = counters_70;
    assign ctr_array[71] = counters_71;
    assign ctr_array[72] = counters_72;
    assign ctr_array[73] = counters_73;
    assign ctr_array[74] = counters_74;
    // L2 cache counters
    assign ctr_array[75] = counters_75;
    assign ctr_array[76] = counters_76;
    assign ctr_array[77] = counters_77;
    assign ctr_array[78] = counters_78;
    assign ctr_array[79] = counters_79;
    assign ctr_array[80] = counters_80;
    assign ctr_array[81] = counters_81;
    assign ctr_array[82] = counters_82;
    assign ctr_array[83] = counters_83;
    assign ctr_array[84] = counters_84;
    assign ctr_array[85] = counters_85;
    assign ctr_array[86] = counters_86;
    assign ctr_array[87] = counters_87;
    assign ctr_array[88] = counters_88;
    assign ctr_array[89] = counters_89;
    assign ctr_array[90] = counters_90;
    assign ctr_array[91] = counters_91;
    // OOO engine counters
    assign ctr_array[92] = counters_92;
    assign ctr_array[93] = counters_93;
    assign ctr_array[94] = counters_94;
    assign ctr_array[95] = counters_95;
    assign ctr_array[96] = counters_96;
    assign ctr_array[97] = counters_97;
    assign ctr_array[98] = counters_98;
    // Fetch/decode counters
    assign ctr_array[99] = counters_99;
    // L3 TMA counters
    assign ctr_array[100] = counters_100;
    assign ctr_array[101] = counters_101;
    assign ctr_array[102] = counters_102;
    assign ctr_array[103] = counters_103;
    assign ctr_array[104] = counters_104;
    assign ctr_array[105] = counters_105;
    assign ctr_array[106] = counters_106;
    assign ctr_array[107] = counters_107;
    assign ctr_array[108] = counters_108;
    // L2 extra counter
    assign ctr_array[109] = counters_109;

    initial begin
        enabled = $test$plusargs("dump-tma-counters");
    end

    integer i;
    always @(posedge clock) begin
        if (!reset && enabled) begin
            for (i = 0; i < NUM_COUNTERS; i = i + 1) begin
                tma_counter_store(TILE_ID, i, ctr_array[i]);
            end
        end
    end

    final begin
        if (enabled) begin
            tma_counter_dump_final(TILE_ID);
        end
    end

endmodule
