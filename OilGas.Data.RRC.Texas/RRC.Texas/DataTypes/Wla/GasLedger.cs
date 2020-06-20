#nullable enable
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
	[Serializable]
	[DataContract]
	[XmlRoot(nameof(GasLedger))]
	[Table(nameof(GasLedger))]
	public class GasLedger	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(REC_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? REC_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 FLD-CODE.

		//05 DIST.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DIST_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DIST_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DIST_SFX), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DIST_SFX { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PERM_FLD_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PERM_FLD_ID { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 FLD-ID REDEFINES PERM-FLD-ID.

		//07 FLD-NAM PIC 9(5).

		//07 FLD-RES PIC 999.

		//03 NEXT-FLD REDEFINES FLD-CODE PIC X(11).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FLD_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? FLD_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? COUNTY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 COUNTIES REDEFINES COUNTY OCCURS 6 TIMES

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DISC_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DISC_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 D-DATE REDEFINES DISC-DATE.

		//05 DISC-DATE-CCYY PIC 9(4).

		//05 DISC-DATE-CCYY-REDF REDEFINES DISC-DATE-CCYY.

		//07 CENT PIC 99.

		//07 YEAR PIC 99.

		//05 MONTH PIC 99.

		//05 DA PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TYPE_F), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? TYPE_F { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CLASS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? CLASS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ALLO_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? ALLO_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SPOCE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? SPOCE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 SPACING REDEFINES SPOCE.

		//05 LINE-SP PIC 9(4).

		//05 WELL-SP PIC 9(4).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(NET_ALLO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? NET_ALLO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BAL_RULE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? BAL_RULE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		// 03 XMT-FACT PIC 9. 103

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PRINT_AS_IS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? PRINT_AS_IS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COL_HEAD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? COL_HEAD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(T_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? T_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 TEST-CODE REDEFINES T-CODE OCCURS 12 TIMES

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CUMU_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CUMU_PROD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CUMU_COND_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CUMU_COND_PROD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(UNIT_ACRES), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? UNIT_ACRES { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(UNIT_TOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? UNIT_TOL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ALLOCTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? ALLOCTION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CASING), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? CASING { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DIAGONAL.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DIAG_X), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DIAG_X { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DIAG_INFO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DIAG_INFO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FG_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? FG_DEPTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELLS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? WELLS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ALLOW_CALC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? ALLOW_CALC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? TOL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ALLOW_DESIRED), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? ALLOW_DESIRED { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TOTAL_FORECAST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? TOTAL_FORECAST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OFFSHORE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? OFFSHORE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FLD_TRANS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? FLD_TRANS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EX_BAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? EX_BAL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EX_GOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? EX_GOR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PENDING_SPEC_LMT_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? PENDING_SPEC_LMT_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CUMU_PROD_PRIOR_70), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CUMU_PROD_PRIOR_70 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CUMU_PROD_ERROR_SW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? CUMU_PROD_ERROR_SW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 LINE-INFO OCCURS 4 TIMES PIC X(66). 303

		//03 FLD-MONTH OCCURS 14 TIMES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FLD_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? FLD_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 FLD-DATE-REDF REDEFINES FLD-DATE.

		//07 FLD-DATE-CCYY PIC 9(4).

		//07 FLD-DATE-CCYY-REDF REDEFINES FLD-DATE-CCYY.

		//09 F-CENT PIC 99.

		//09 F-YEAR PIC 99.

		//07 F-MONTH PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_CHANGE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? F_CHANGE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PER_WELL_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? PER_WELL_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PER_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PER_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(AC_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? AC_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ACRG_FACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? ACRG_FACT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OTHER_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? OTHER_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OTHER_FACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? OTHER_FACT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SW_SPLIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? SW_SPLIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SPLIT_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? SPLIT_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SPECIAL_LMT_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? SPECIAL_LMT_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SW_EXC_206_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? SW_EXC_206_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SW_EXC_8609_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? SW_EXC_8609_LIMIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//01 WELL-REC.

		// 03 W-REC-CODE PIC 9. 1

		//03 WELL-CODE.

		//05 OP-CODE.

		//07 W-FLD-CODE.

		//09 W-DISTR.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DIST_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? W_DIST_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DIST_SFX), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? W_DIST_SFX { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_PERM_FLD_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_PERM_FLD_ID { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//09 W-FLD-ID REDEFINES W-PERM-FLD-ID.

		//11 W-FLD-NAM PIC 9(5).

		//11 W-FLD-RES PIC 999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OPER_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OPER_ID { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WELL_ID { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 NEXT-WELL REDEFINES WELL-CODE PIC X(23).

		//03 WELL-NO.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TRACT_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? TRACT_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_NR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? WELL_NR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 WELL-SFX.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SFX_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? SFX_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SFX_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? SFX_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LSE_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? LSE_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CO_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? CO_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? W_TYPE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(GAS_GATHER_OLD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? GAS_GATHER_OLD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 GAS-GATHER-NEW.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(G_GATH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? G_GATH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FULL_S), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? FULL_S { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(GAS_SPLIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? GAS_SPLIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(GASGATH2I), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? GASGATH2I { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(GASGATH3I), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? GASGATH3I { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LIQ_GATHER_OLD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? LIQ_GATHER_OLD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 LIQ-GATHER-NEW.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_GATH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? L_GATH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LIQ_SPLIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? LIQ_SPLIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_INFO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? WELL_INFO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BATCH_NR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? BATCH_NR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EXC_14B), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? EXC_14B { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(_14B_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? _14B_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 14B-DATE-REDF REDEFINES 14B-DATE.

		//05 14B-CCYY PIC 9(4).

		//05 14B-CCYY-REDF REDEFINES 14B-CCYY.

		//07 14B-CC PIC 99.

		//07 14B-YR PIC 99.

		//05 14B-MO PIC 99.

		//05 14B-DAY PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CMP_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CMP_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 CMP-DATE-REDF REDEFINES CMP-DATE.

		//05 CMP-DATE-CCYY PIC 9(4).

		//05 CMP-DATE-CCYY-REDF REDEFINES CMP-DATE-CCYY.

		//07 CMP-DATE-CC PIC 99.

		//07 CMP-DATE-YY PIC 99.

		//05 CMP-DATE-MM PIC 99.

		//05 CMP-DATE-DD PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_DEPTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		// 03 UP-PERF PIC 9(5). 146

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LO_PERF), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LO_PERF { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COMMCD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? COMMCD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COMM), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? COMM { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COMN_DTE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? COMN_DTE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 COMN-DTE-REDF REDEFINES COMN-DTE.

		//05 COMN-CCYY PIC 9(4).

		//05 COMN-CCYY-REDF REDEFINES COMN-CCYY.

		//07 COMN-CC PIC 99.

		//07 COMN-YR PIC 99.

		//05 COMN-M PIC 99.

		//05 COMN-DAY PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DPT_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DPT_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BHP_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? BHP_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SIP_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? SIP_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(G_4_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? G_4_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WL_TSTX), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? WL_TSTX { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(G_10_DUE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? G_10_DUE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DTE_L_UTL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DTE_L_UTL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DTE-L-UTL-REDF REDEFINES DTE-L-UTL.

		//05 DTE-L-UTL-CCYY PIC 9(4).

		//05 DTE-L-UTL-CCYY-REDF REDEFINES DTE-L-UTL-CCYY.

		//07 DTE-L-UTL-CC PIC 99.

		//07 DTE-L-UTL-YY PIC 99.

		//05 DTE-L-UTL-MM PIC 99.

		//05 DTE-L-UTL-DD PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WL_PA_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WL_PA_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(P_A_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? P_A_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 P-A-DATE-REDF REDEFINES P-A-DATE.

		//05 P-A-DATE-CCYY PIC 9(4).

		//05 P-A-DATE-CCYY-REDF REDEFINES P-A-DATE-CCYY.

		//07 P-A-DATE-CC PIC 99.

		//07 P-A-DATE-YY PIC 99.

		//05 P-A-DATE-MM PIC 99.

		//05 P-A-DATE-DD PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SP_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? SP_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SP_AL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? SP_AL_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LIQ_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LIQ_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LIQ_ALLOW_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LIQ_ALLOW_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FORM_LACKING), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? FORM_LACKING { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OFF_SHORE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? OFF_SHORE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WL_TOP_PER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WL_TOP_PER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ROYALTY_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? ROYALTY_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PRIOR_RINU), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PRIOR_RINU { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PRIOR_RINU_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PRIOR_RINU_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 PRIOR-RINU-DATE-REDF REDEFINES PRIOR-RINU-DATE.

		//05 PRIOR-RINU-CCYY PIC 9(4).

		//05 PRIOR-RINU-CCYY-REDF REDEFINES PRIOR-RINU-CCYY.

		//07 PRIOR-RINU-CENT PIC 9(2).

		//07 PRIOR-RINU-YEAR PIC 9(2).

		//05 PRIOR-RINU-MONTH PIC 9(2).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(RINU), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? RINU { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(RINU_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? RINU_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 RINU-DATE-REDF REDEFINES RINU-DATE.

		//05 RINU-CCYY PIC 9(4).

		// 05 RINU-CCYY-REDF REDEFINES RINU-CCYY.

		//07 RINU-CENT PIC 9(2).

		//07 RINU-YEAR PIC 9(2).

		//05 RINU-MONTH PIC 9(2).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EXC_14B2_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? EXC_14B2_TYPE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(G_1_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? G_1_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 G-1-DATE-REDF REDEFINES G-1-DATE.

		//05 G-1-DATE-CCYY PIC 9(4).

		//05 G-1-DATE-CCYY-REDF REDEFINES G-1-DATE-CCYY.

		//07 G-1-DATE-CENT PIC 99.

		//07 G-1-DATE-YEAR PIC 99.

		//05 G-1-DATE-MONTH PIC 99.

		//05 G-1-DATE-DAY PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OPEN_FLO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OPEN_FLO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SLOPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? SLOPE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 SLOPED REDEFINES SLOPE PIC S9(5) COMP-3.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? RATIO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(G_GRAV), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? G_GRAV { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 G-GRAVED REDEFINES G-GRAV PIC S999 COMP-3.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(O_GRAV), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? O_GRAV { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 O-GRAVED REDEFINES O-GRAV PIC S999 COMP-3.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BGS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? BGS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BLS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? BLS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PRIOR_6), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PRIOR_6 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PCU), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PCU { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CURR_BAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CURR_BAL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CANCEL_G), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CANCEL_G { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(G_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? G_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? L_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LMT_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LMT_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SI_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? SI_AMT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SI_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? SI_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EXCEPTION_TO_PROD_LIMIT_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? EXCEPTION_TO_PROD_LIMIT_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EXCEPTION_TO_PROD_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? EXCEPTION_TO_PROD_LIMIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EXC_14B2_APP_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? EXC_14B2_APP_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 MONTHLY-WELL OCCURS 14 TIMES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 W-DATE-REDF REDEFINES W-DATE.

		//07 W-DATE-CCYY PIC 9(4).

		//07 W-DATE-CCYY-REDF REDEFINES W-DATE-CCYY.

		//09 W-CC PIC 99.

		//09 W-YR PIC 99.

		//07 W-MO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(NAME_CHANGE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? NAME_CHANGE_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FORM_REC_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? FORM_REC_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BAL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? BAL_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EXC_206_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? EXC_206_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(RED_RATE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? RED_RATE_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TSF_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? TSF_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(NO_SUPP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? NO_SUPP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LACK), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LACK { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OLD_OP_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OLD_OP_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		// 05 W-TYPE-MO PIC X. 337

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(AL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? AL_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WRD_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? WRD_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ON_SHUT_LST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? ON_SHUT_LST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(NO_LMT_ALLOW_SW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? NO_LMT_ALLOW_SW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(GAS_PRD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? GAS_PRD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(INJ_CREDIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? INJ_CREDIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(INJ_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? INJ_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_LIQ_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_LIQ_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LSE_LIQ), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LSE_LIQ { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PLT_LIQ), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PLT_LIQ { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LIQ_DISI), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LIQ_DISI { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LIQ_DISCDI), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LIQ_DISCDI { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OTH_DISI), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OTH_DISI { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OTH_DISCDI), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? OTH_DISCDI { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_G4), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MO_G4 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_G2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MO_G2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MO_BHP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OTHER_PRESI), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OTHER_PRESI { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_ACRE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public float? MO_ACRE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 MO-ACRE-FT PIC S9(6)V9 COMP-3. 396

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TRAN_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? TRAN_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OPEN_COND), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OPEN_COND { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CLOSE_COND), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CLOSE_COND { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_LEASE_PERCENT_RESERVE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public float? MO_LEASE_PERCENT_RESERVE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PER_CENT_RED_RATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PER_CENT_RED_RATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_TOP_SCH_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MO_TOP_SCH_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_LMT_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MO_LMT_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MO_HIGHEST_DAILY_PROD_LMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MO_HIGHEST_DAILY_PROD_LMT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EXC_8609_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? EXC_8609_LIMIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SWR38_ACRES_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? SWR38_ACRES_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public GasLedger()
		{
		}

		public GasLedger(ReadOnlySpan<byte> source)
		{
			REC_CODE              = StringParser.ReadAsSByte(source, 1 - 1, 1);
			//03 FLD-CODE.
			//05 DIST.
			DIST_NO              = StringParser.ReadAsInt16(source, 2 - 1, 2);
			DIST_SFX              = StringParser.ReadAsChar(source, 4 - 1, 1);
			PERM_FLD_ID              = StringParser.ReadAsInt64(source, 5 - 1, 8);
			//05 FLD-ID REDEFINES PERM-FLD-ID.
			//07 FLD-NAM PIC 9(5).
			//07 FLD-RES PIC 999.
			//03 NEXT-FLD REDEFINES FLD-CODE PIC X(11).
			FLD_NAME              = StringParser.ReadAsString(source, 32 - 1, 32);
			COUNTY              = StringParser.ReadAsInt64(source, 64 - 1, 18);
			//03 COUNTIES REDEFINES COUNTY OCCURS 6 TIMES
			DISC_DATE              = StringParser.ReadAsInt64(source, 82 - 1, 8);
			//03 D-DATE REDEFINES DISC-DATE.
			//05 DISC-DATE-CCYY PIC 9(4).
			//05 DISC-DATE-CCYY-REDF REDEFINES DISC-DATE-CCYY.
			//07 CENT PIC 99.
			//07 YEAR PIC 99.
			//05 MONTH PIC 99.
			//05 DA PIC 99.
			TYPE_F              = StringParser.ReadAsChar(source, 90 - 1, 1);
			CLASS              = StringParser.ReadAsChar(source, 91 - 1, 1);
			ALLO_CD              = StringParser.ReadAsChar(source, 92 - 1, 1);
			SPOCE              = StringParser.ReadAsInt64(source, 93 - 1, 8);
			//03 SPACING REDEFINES SPOCE.
			//05 LINE-SP PIC 9(4).
			//05 WELL-SP PIC 9(4).
			NET_ALLO              = StringParser.ReadAsSByte(source, 101 - 1, 1);
			BAL_RULE              = StringParser.ReadAsSByte(source, 102 - 1, 2);
			// 03 XMT-FACT PIC 9. 103
			PRINT_AS_IS              = StringParser.ReadAsSByte(source, 104 - 1, 1);
			COL_HEAD              = StringParser.ReadAsChar(source, 105 - 1, 1);
			T_CODE              = StringParser.ReadAsString(source, 106 - 1, 12);
			//03 TEST-CODE REDEFINES T-CODE OCCURS 12 TIMES
			CUMU_PROD              = StringParser.ReadAsInt64(source, 118 - 1, 12);
			CUMU_COND_PROD              = StringParser.ReadAsInt64(source, 130 - 1, 10);
			UNIT_ACRES              = StringParser.ReadAsInt32(source, 140 - 1, 4);
			UNIT_TOL              = StringParser.ReadAsInt32(source, 144 - 1, 3);
			ALLOCTION              = StringParser.ReadAsInt64(source, 147 - 1, 15);
			CASING              = StringParser.ReadAsString(source, 162 - 1, 21);
			//03 DIAGONAL.
			DIAG_X              = StringParser.ReadAsChar(source, 183 - 1, 1);
			DIAG_INFO              = StringParser.ReadAsString(source, 184 - 1, 20);
			FG_DEPTH              = StringParser.ReadAsInt64(source, 204 - 1, 5);
			WELLS              = StringParser.ReadAsInt32(source, 209 - 1, 4);
			ALLOW_CALC              = StringParser.ReadAsInt64(source, 213 - 1, 8);
			TOL              = StringParser.ReadAsInt32(source, 221 - 1, 3);
			ALLOW_DESIRED              = StringParser.ReadAsInt64(source, 224 - 1, 8);
			TOTAL_FORECAST              = StringParser.ReadAsInt64(source, 232 - 1, 8);
			OFFSHORE              = StringParser.ReadAsSByte(source, 240 - 1, 1);
			FLD_TRANS              = StringParser.ReadAsSByte(source, 241 - 1, 1);
			EX_BAL              = StringParser.ReadAsSByte(source, 242 - 1, 1);
			EX_GOR              = StringParser.ReadAsSByte(source, 243 - 1, 1);
			PENDING_SPEC_LMT_ALLOW              = StringParser.ReadAsSByte(source, 244 - 1, 1);
			CUMU_PROD_PRIOR_70              = StringParser.ReadAsInt64(source, 245 - 1, 12);
			CUMU_PROD_ERROR_SW              = StringParser.ReadAsChar(source, 257 - 1, 1);
			//03 LINE-INFO OCCURS 4 TIMES PIC X(66). 303
			//03 FLD-MONTH OCCURS 14 TIMES.
			FLD_DATE              = StringParser.ReadAsInt64(source, 583 - 1, 6);
			//05 FLD-DATE-REDF REDEFINES FLD-DATE.
			//07 FLD-DATE-CCYY PIC 9(4).
			//07 FLD-DATE-CCYY-REDF REDEFINES FLD-DATE-CCYY.
			//09 F-CENT PIC 99.
			//09 F-YEAR PIC 99.
			//07 F-MONTH PIC 99.
			F_CHANGE              = StringParser.ReadAsSByte(source, 589 - 1, 1);
			PER_WELL_CD              = StringParser.ReadAsInt16(source, 590 - 1, 2);
			PER_WELL              = StringParser.ReadAsPackedInt64(source, 592 - 1, 4);
			AC_CD              = StringParser.ReadAsInt16(source, 596 - 1, 2);
			ACRG_FACT              = StringParser.ReadAsPackedDouble(source, 7, 598 - 1, 8);
			OTHER_CD              = StringParser.ReadAsInt16(source, 606 - 1, 2);
			OTHER_FACT              = StringParser.ReadAsPackedDouble(source, 7, 608 - 1, 6);
			SW_SPLIT              = StringParser.ReadAsPackedInt64(source, 614 - 1, 2);
			SPLIT_DATE              = StringParser.ReadAsInt16(source, 616 - 1, 2);
			SPECIAL_LMT_ALLOW              = StringParser.ReadAsSByte(source, 618 - 1, 1);
			SW_EXC_206_CODE              = StringParser.ReadAsSByte(source, 619 - 1, 1);
			SW_EXC_8609_LIMIT              = StringParser.ReadAsInt16(source, 620 - 1, 2);
			//01 WELL-REC.
			// 03 W-REC-CODE PIC 9. 1
			//03 WELL-CODE.
			//05 OP-CODE.
			//07 W-FLD-CODE.
			//09 W-DISTR.
			W_DIST_NO              = StringParser.ReadAsInt16(source, 2 - 1, 2);
			W_DIST_SFX              = StringParser.ReadAsChar(source, 4 - 1, 1);
			W_PERM_FLD_ID              = StringParser.ReadAsInt64(source, 5 - 1, 8);
			//09 W-FLD-ID REDEFINES W-PERM-FLD-ID.
			//11 W-FLD-NAM PIC 9(5).
			//11 W-FLD-RES PIC 999.
			OPER_ID              = StringParser.ReadAsInt64(source, 13 - 1, 6);
			WELL_ID              = StringParser.ReadAsInt64(source, 19 - 1, 6);
			//03 NEXT-WELL REDEFINES WELL-CODE PIC X(23).
			//03 WELL-NO.
			TRACT_NO              = StringParser.ReadAsChar(source, 26 - 1, 1);
			WELL_NR              = StringParser.ReadAsString(source, 27 - 1, 3);
			//05 WELL-SFX.
			SFX_1              = StringParser.ReadAsChar(source, 30 - 1, 1);
			SFX_2              = StringParser.ReadAsChar(source, 31 - 1, 1);
			LSE_NAME              = StringParser.ReadAsString(source, 32 - 1, 32);
			CO_CODE              = StringParser.ReadAsInt32(source, 64 - 1, 3);
			W_TYPE              = StringParser.ReadAsChar(source, 67 - 1, 1);
			GAS_GATHER_OLD              = StringParser.ReadAsString(source, 68 - 1, 5);
			//03 GAS-GATHER-NEW.
			G_GATH              = StringParser.ReadAsString(source, 73 - 1, 5);
			FULL_S              = StringParser.ReadAsChar(source, 78 - 1, 1);
			GAS_SPLIT              = StringParser.ReadAsChar(source, 79 - 1, 1);
			GASGATH2I              = StringParser.ReadAsString(source, 80 - 1, 5);
			GASGATH3I              = StringParser.ReadAsString(source, 85 - 1, 5);
			LIQ_GATHER_OLD              = StringParser.ReadAsString(source, 90 - 1, 5);
			//03 LIQ-GATHER-NEW.
			L_GATH              = StringParser.ReadAsString(source, 95 - 1, 5);
			LIQ_SPLIT              = StringParser.ReadAsChar(source, 100 - 1, 1);
			WELL_INFO              = StringParser.ReadAsString(source, 101 - 1, 22);
			BATCH_NR              = StringParser.ReadAsChar(source, 123 - 1, 1);
			EXC_14B              = StringParser.ReadAsChar(source, 124 - 1, 1);
            _14B_DATE = StringParser.ReadAsInt64(source, 125 - 1, 8);
			//03 14B-DATE-REDF REDEFINES 14B-DATE.
			//05 14B-CCYY PIC 9(4).
			//05 14B-CCYY-REDF REDEFINES 14B-CCYY.
			//07 14B-CC PIC 99.
			//07 14B-YR PIC 99.
			//05 14B-MO PIC 99.
			//05 14B-DAY PIC 99.
			CMP_DATE              = StringParser.ReadAsInt64(source, 133 - 1, 8);
			//03 CMP-DATE-REDF REDEFINES CMP-DATE.
			//05 CMP-DATE-CCYY PIC 9(4).
			//05 CMP-DATE-CCYY-REDF REDEFINES CMP-DATE-CCYY.
			//07 CMP-DATE-CC PIC 99.
			//07 CMP-DATE-YY PIC 99.
			//05 CMP-DATE-MM PIC 99.
			//05 CMP-DATE-DD PIC 99.
			W_DEPTH              = StringParser.ReadAsInt64(source, 141 - 1, 10);
			// 03 UP-PERF PIC 9(5). 146
			LO_PERF              = StringParser.ReadAsInt64(source, 151 - 1, 5);
			COMMCD              = StringParser.ReadAsSByte(source, 156 - 1, 1);
			COMM              = StringParser.ReadAsInt32(source, 157 - 1, 4);
			COMN_DTE              = StringParser.ReadAsInt64(source, 161 - 1, 8);
			//03 COMN-DTE-REDF REDEFINES COMN-DTE.
			//05 COMN-CCYY PIC 9(4).
			//05 COMN-CCYY-REDF REDEFINES COMN-CCYY.
			//07 COMN-CC PIC 99.
			//07 COMN-YR PIC 99.
			//05 COMN-M PIC 99.
			//05 COMN-DAY PIC 99.
			DPT_CODE              = StringParser.ReadAsChar(source, 169 - 1, 1);
			BHP_CODE              = StringParser.ReadAsChar(source, 170 - 1, 1);
			SIP_CODE              = StringParser.ReadAsChar(source, 171 - 1, 1);
			G_4_CODE              = StringParser.ReadAsChar(source, 172 - 1, 1);
			WL_TSTX              = StringParser.ReadAsChar(source, 173 - 1, 1);
			G_10_DUE              = StringParser.ReadAsInt16(source, 174 - 1, 2);
			DTE_L_UTL              = StringParser.ReadAsInt64(source, 176 - 1, 8);
			//03 DTE-L-UTL-REDF REDEFINES DTE-L-UTL.
			//05 DTE-L-UTL-CCYY PIC 9(4).
			//05 DTE-L-UTL-CCYY-REDF REDEFINES DTE-L-UTL-CCYY.
			//07 DTE-L-UTL-CC PIC 99.
			//07 DTE-L-UTL-YY PIC 99.
			//05 DTE-L-UTL-MM PIC 99.
			//05 DTE-L-UTL-DD PIC 99.
			WL_PA_CD              = StringParser.ReadAsSByte(source, 184 - 1, 1);
			P_A_DATE              = StringParser.ReadAsInt64(source, 185 - 1, 8);
			//03 P-A-DATE-REDF REDEFINES P-A-DATE.
			//05 P-A-DATE-CCYY PIC 9(4).
			//05 P-A-DATE-CCYY-REDF REDEFINES P-A-DATE-CCYY.
			//07 P-A-DATE-CC PIC 99.
			//07 P-A-DATE-YY PIC 99.
			//05 P-A-DATE-MM PIC 99.
			//05 P-A-DATE-DD PIC 99.
			SP_ALLOW              = StringParser.ReadAsInt64(source, 193 - 1, 7);
			SP_AL_CODE              = StringParser.ReadAsChar(source, 200 - 1, 1);
			LIQ_ALLOW              = StringParser.ReadAsInt64(source, 201 - 1, 5);
			LIQ_ALLOW_CODE              = StringParser.ReadAsSByte(source, 206 - 1, 1);
			FORM_LACKING              = StringParser.ReadAsSByte(source, 207 - 1, 1);
			OFF_SHORE              = StringParser.ReadAsSByte(source, 208 - 1, 1);
			WL_TOP_PER              = StringParser.ReadAsInt64(source, 209 - 1, 5);
			ROYALTY_CODE              = StringParser.ReadAsSByte(source, 214 - 1, 1);
			PRIOR_RINU              = StringParser.ReadAsPackedInt64(source, 215 - 1, 5);
			PRIOR_RINU_DATE              = StringParser.ReadAsInt64(source, 220 - 1, 6);
			//03 PRIOR-RINU-DATE-REDF REDEFINES PRIOR-RINU-DATE.
			//05 PRIOR-RINU-CCYY PIC 9(4).
			//05 PRIOR-RINU-CCYY-REDF REDEFINES PRIOR-RINU-CCYY.
			//07 PRIOR-RINU-CENT PIC 9(2).
			//07 PRIOR-RINU-YEAR PIC 9(2).
			//05 PRIOR-RINU-MONTH PIC 9(2).
			RINU              = StringParser.ReadAsPackedInt64(source, 226 - 1, 5);
			RINU_DATE              = StringParser.ReadAsInt64(source, 231 - 1, 6);
			//03 RINU-DATE-REDF REDEFINES RINU-DATE.
			//05 RINU-CCYY PIC 9(4).
			// 05 RINU-CCYY-REDF REDEFINES RINU-CCYY.
			//07 RINU-CENT PIC 9(2).
			//07 RINU-YEAR PIC 9(2).
			//05 RINU-MONTH PIC 9(2).
			EXC_14B2_TYPE              = StringParser.ReadAsChar(source, 237 - 1, 1);
			G_1_DATE              = StringParser.ReadAsInt64(source, 239 - 1, 8);
			//03 G-1-DATE-REDF REDEFINES G-1-DATE.
			//05 G-1-DATE-CCYY PIC 9(4).
			//05 G-1-DATE-CCYY-REDF REDEFINES G-1-DATE-CCYY.
			//07 G-1-DATE-CENT PIC 99.
			//07 G-1-DATE-YEAR PIC 99.
			//05 G-1-DATE-MONTH PIC 99.
			//05 G-1-DATE-DAY PIC 99.
			OPEN_FLO              = StringParser.ReadAsPackedInt64(source, 247 - 1, 4);
			SLOPE              = StringParser.ReadAsPackedInt64(source, 251 - 1, 3);
			//03 SLOPED REDEFINES SLOPE PIC S9(5) COMP-3.
			RATIO              = StringParser.ReadAsPackedInt64(source, 254 - 1, 3);
			G_GRAV              = StringParser.ReadAsPackedInt64(source, 257 - 1, 2);
			//03 G-GRAVED REDEFINES G-GRAV PIC S999 COMP-3.
			O_GRAV              = StringParser.ReadAsPackedInt64(source, 259 - 1, 2);
			//03 O-GRAVED REDEFINES O-GRAV PIC S999 COMP-3.
			BGS              = StringParser.ReadAsPackedInt64(source, 261 - 1, 5);
			BLS              = StringParser.ReadAsPackedInt64(source, 266 - 1, 5);
			PRIOR_6              = StringParser.ReadAsPackedInt64(source, 271 - 1, 5);
			PCU              = StringParser.ReadAsPackedInt64(source, 276 - 1, 4);
			CURR_BAL              = StringParser.ReadAsPackedInt64(source, 280 - 1, 5);
			CANCEL_G              = StringParser.ReadAsPackedInt64(source, 285 - 1, 4);
			G_STATUS              = StringParser.ReadAsPackedInt64(source, 289 - 1, 5);
			L_STATUS              = StringParser.ReadAsPackedInt64(source, 294 - 1, 4);
			LMT_ALLOW              = StringParser.ReadAsPackedInt64(source, 298 - 1, 4);
			SI_AMT              = StringParser.ReadAsPackedInt64(source, 302 - 1, 5);
			SI_CODE              = StringParser.ReadAsSByte(source, 307 - 1, 1);
			EXCEPTION_TO_PROD_LIMIT_CODE              = StringParser.ReadAsSByte(source, 308 - 1, 1);
			EXCEPTION_TO_PROD_LIMIT              = StringParser.ReadAsPackedInt64(source, 309 - 1, 4);
			EXC_14B2_APP_NO              = StringParser.ReadAsPackedInt64(source, 313 - 1, 4);
			//03 MONTHLY-WELL OCCURS 14 TIMES.
			W_DATE              = StringParser.ReadAsInt64(source, 317 - 1, 6);
			//05 W-DATE-REDF REDEFINES W-DATE.
			//07 W-DATE-CCYY PIC 9(4).
			//07 W-DATE-CCYY-REDF REDEFINES W-DATE-CCYY.
			//09 W-CC PIC 99.
			//09 W-YR PIC 99.
			//07 W-MO PIC 99.
			NAME_CHANGE_CODE              = StringParser.ReadAsSByte(source, 323 - 1, 1);
			FORM_REC_CODE              = StringParser.ReadAsSByte(source, 324 - 1, 1);
			BAL_CODE              = StringParser.ReadAsSByte(source, 325 - 1, 1);
			EXC_206_CODE              = StringParser.ReadAsSByte(source, 326 - 1, 1);
			RED_RATE_CODE              = StringParser.ReadAsSByte(source, 327 - 1, 1);
			TSF_CODE              = StringParser.ReadAsSByte(source, 328 - 1, 1);
			NO_SUPP              = StringParser.ReadAsSByte(source, 329 - 1, 1);
			LACK              = StringParser.ReadAsSByte(source, 330 - 1, 1);
			OLD_OP_CODE              = StringParser.ReadAsInt64(source, 331 - 1, 7);
			// 05 W-TYPE-MO PIC X. 337
			AL_CODE              = StringParser.ReadAsChar(source, 338 - 1, 1);
			WRD_ALLOW              = StringParser.ReadAsString(source, 339 - 1, 8);
			ON_SHUT_LST              = StringParser.ReadAsChar(source, 347 - 1, 1);
			NO_LMT_ALLOW_SW              = StringParser.ReadAsSByte(source, 348 - 1, 1);
			ALLOW              = StringParser.ReadAsPackedInt64(source, 349 - 1, 4);
			GAS_PRD              = StringParser.ReadAsPackedInt64(source, 353 - 1, 4);
			INJ_CREDIT              = StringParser.ReadAsPackedInt64(source, 357 - 1, 4);
			INJ_CODE              = StringParser.ReadAsSByte(source, 361 - 1, 1);
			W_LIQ_ALLOW              = StringParser.ReadAsPackedInt64(source, 362 - 1, 3);
			LSE_LIQ              = StringParser.ReadAsPackedInt64(source, 365 - 1, 3);
			PLT_LIQ              = StringParser.ReadAsPackedInt64(source, 368 - 1, 3);
			LIQ_DISI              = StringParser.ReadAsPackedInt64(source, 371 - 1, 3);
			LIQ_DISCDI              = StringParser.ReadAsSByte(source, 374 - 1, 1);
			OTH_DISI              = StringParser.ReadAsPackedInt64(source, 375 - 1, 3);
			OTH_DISCDI              = StringParser.ReadAsSByte(source, 378 - 1, 1);
			MO_G4              = StringParser.ReadAsPackedInt64(source, 379 - 1, 4);
			MO_G2              = StringParser.ReadAsPackedInt64(source, 383 - 1, 3);
			MO_BHP              = StringParser.ReadAsPackedInt64(source, 386 - 1, 3);
			OTHER_PRESI              = StringParser.ReadAsPackedInt64(source, 389 - 1, 3);
			MO_ACRE              = StringParser.ReadAsPackedSingle(source, 3, 392 - 1, 8);
			//05 MO-ACRE-FT PIC S9(6)V9 COMP-3. 396
			TRAN_ALLOW              = StringParser.ReadAsPackedInt64(source, 400 - 1, 4);
			OPEN_COND              = StringParser.ReadAsPackedInt64(source, 404 - 1, 3);
			CLOSE_COND              = StringParser.ReadAsPackedInt64(source, 407 - 1, 3);
			MO_LEASE_PERCENT_RESERVE              = StringParser.ReadAsPackedSingle(source, 4, 410 - 1, 4);
			PER_CENT_RED_RATE              = StringParser.ReadAsPackedInt64(source, 414 - 1, 4);
			MO_TOP_SCH_ALLOW              = StringParser.ReadAsPackedInt64(source, 418 - 1, 4);
			MO_LMT_ALLOW              = StringParser.ReadAsPackedInt64(source, 422 - 1, 4);
			MO_HIGHEST_DAILY_PROD_LMT              = StringParser.ReadAsPackedInt64(source, 426 - 1, 4);
			EXC_8609_LIMIT              = StringParser.ReadAsInt16(source, 430 - 1, 2);
			SWR38_ACRES_CODE              = StringParser.ReadAsChar(source, 432 - 1, 1509);
		}
	}
}
