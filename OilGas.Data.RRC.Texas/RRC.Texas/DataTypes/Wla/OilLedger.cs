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
	[XmlRoot(nameof(OilLedger))]
	[Table(nameof(OilLedger))]
	public class OilLedger	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 OIL-CODES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TYPE_REC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? TYPE_REC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DIST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FIELD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? FIELD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OPR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OPR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LEASE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LEASE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OFFSHORE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? OFFSHORE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 FIELD-DATA.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? F_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? COUNTY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 COUNTIES REDEFINES COUNTY

		//OCCURS 6 TIMES

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DISC_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DISC_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 D-DATE REDEFINES DISC-DATE.

		//05 DSC-CCYY PIC 9(4).

		//05 DSC-CCYY-REDF REDEFINES DSC-CCYY.

		//07 DSC-CC PIC 99.

		//07 DSC-YR PIC 99.

		//05 DSC-MO PIC 99.

		//05 DSC-DAY PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? F_DEPTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(O_GRAV), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? O_GRAV { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? F_TYPE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MULT_RES), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MULT_RES { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_LPB), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? F_LPB { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_XMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? F_XMT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PRT_AS_IS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? PRT_AS_IS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(YARD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? YARD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(T_CODES), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? T_CODES { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 TEST-CODE REDEFINES T-CODES

		//OCCURS 12 TIMES

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ALLOCATION), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? ALLOCATION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 FACTORS REDEFINES ALLOCATION

		//OCCURS 3 TIMES.

		//05 PERCNT PIC V99.

		//05 CODEF PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(RES_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? RES_AMT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_GOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? F_GOR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 F-LIMIT REDEFINES F-GOR.

		//05 F-GOR-CODE PIC 9.

		//05 F-GOR-AMT PIC 9(5).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_TOP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? F_TOP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 F-MER REDEFINES F-TOP.

		//05 F-TOP-CODE PIC 9.

		//05 F-TOP-AMT PIC 9(4).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_NET), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? F_NET { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 F-NGOR REDEFINES F-NET.

		//05 F-NET-CODE PIC 9.

		//05 F-NET-AMT PIC 9(5).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(UNET), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? UNET { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? TOL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 TOLER REDEFINES TOL.

		//05 F-TOL-CODE PIC 9.

		//05 F-TOL-AMT PIC 999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SPAC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? SPAC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 SPAC1-2 REDEFINES SPAC.

		//05 SPAC1 PIC 9999.

		//05 SPAC2 PIC 9999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DIAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? DIAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CUM_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? CUM_PROD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CASING), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? CASING { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COL_HEAD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? COL_HEAD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(ALO_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? ALO_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_RMK1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? F_RMK1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_RMK2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? F_RMK2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PERM_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? PERM_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SP_FHC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? SP_FHC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 ANNUAL-1.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(AN_A), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? AN_A { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(AN_B), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? AN_B { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 ANNUAL-2 REDEFINES ANNUAL-1.

		//05 AN-1 PIC X(66).

		//05 AN-2 PIC X(59).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(F_OOIP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? F_OOIP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 F-MONTH OCCURS 14 TIMES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? FM_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 FM-DATE-REDF REDEFINES FM-DATE.

		//07 FM-CCYY PIC 9(4).

		//07 FM-CCYY-REDF REDEFINES FM-CCYY.

		//09 FM-CC PIC 99.

		//09 FM-YR PIC 99.

		//07 FM-MO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_PW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? FM_PW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_AC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? FM_AC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_OTHC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? FM_OTHC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_CHG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? FM_CHG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_PROD_FACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? FM_PROD_FACT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_SPLIT_PROD_FACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? FM_SPLIT_PROD_FACT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 FM-SPLIT-PROD-FACT-DATE

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_JOHN), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? FM_JOHN { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FM_OTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? FM_OTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//01 LEASE-MAST REDEFINES FIELD-MAST.

		//02 LSE-CODES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LEASE_REC_TYPE_REC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LEASE_REC_TYPE_REC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LEASE_REC_DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? LEASE_REC_DIST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LEASE_REC_FIELD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LEASE_REC_FIELD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LEASE_REC_OPR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LEASE_REC_OPR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LEASE_REC_LEASE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LEASE_REC_LEASE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LEASE_REC_OFFSHORE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LEASE_REC_OFFSHORE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 LEASE-DATA.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? L_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LSE_CO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LSE_CO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 LEASE-CO REDEFINES LSE-CO.

		//05 L-CO-1 PIC 999.

		//05 L-CO-2 PIC 999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(POGATH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? POGATH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PGGATH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? PGGATH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OSPLIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? OSPLIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(GSPLIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? GSPLIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OOGATH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? OOGATH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OGGATH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? OGGATH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(OOPR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? OOPR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BO_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? BO_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BG_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? BG_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MOVE_BAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MOVE_BAL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PO_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PO_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PG_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PG_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SEC_REC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? SEC_REC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CERT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? CERT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BATCH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? BATCH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_LPB), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? L_LPB { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COMMINGLE_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? COMMINGLE_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COMMINGLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? COMMINGLE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_INFO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? L_INFO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(AD_BO_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? AD_BO_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(AD_BG_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? AD_BG_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(COMMINGLE_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? COMMINGLE_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 COMMINGLE-DATE-REDF REDEFINES COMMINGLE-DATE.

		//05 COMMINGLE-CCYY PIC 9(4).

		//05 COMMINGLE-CCYY-REDF REDEFINES

		//07 COMMINGLE-CC PIC 99.

		//07 COMMINGLE-YR PIC 99.

		//05 COMMINGLE-MO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_RMCD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? L_RMCD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_RMDT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? L_RMDT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 L-RMDT-REDF REDEFINES L-RMDT.

		//05 L-RMDT-CCYY PIC 9(4).

		//05 L-RMDT-CCYY-REDF REDEFINES L-RMDT-CCYY.

		//07 L-RMCC PIC 99.

		//07 L-RMYR PIC 99.

		//05 L-RMMO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SEV_CD_13), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? SEV_CD_13 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SEV_CD_14), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? SEV_CD_14 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 CAS-RED.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_CAS_SI_LTR_DTE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? L_CAS_SI_LTR_DTE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_RED_RTE_DTE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? L_RED_RTE_DTE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 CAS-RED-A REDEFINES CAS-RED.

		//05 L-CAS-CCYY PIC 9(4).

		//05 L-CAS-CCYY-REDF REDEFINES L-CAS-CCYY.

		//07 L-CAS-CC PIC 99.

		//07 L-CAS-YR PIC 99.

		//05 L-CAS-MO PIC 99.

		//05 L-RED-CCYY PIC 9(4).

		//05 L-RED-CCYY-REDF REDEFINES L-RED-CCYY.

		//07 L-RED-CC PIC 99.

		//07 L-RED-YR PIC 99.

		//05 L-RED-MO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_EXC_TST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? L_EXC_TST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_RLTYCD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? L_RLTYCD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_ONE_WELL_LEASE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? L_ONE_WELL_LEASE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_PANHANDLE_GOR_EXC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? L_PANHANDLE_GOR_EXC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 L-PANHANDLE-GOR-AMT PIC 9(08)V9 COMP-3. 216

		//03 L-MONTH OCCURS 12 TIMES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(L_MONTH_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? L_MONTH_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 L-MONTH-DATE-REDF REDEFINES

		//07 L-MONTH-CCYY PIC 9(4).

		//07 L-MONTH-CCYY-REDF REDEFINES

		//09 LM-CC PIC 99.

		//09 LM-YR PIC 99.

		//07 LM-MO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_SEV), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_SEV { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_RETRO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_RETRO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_REC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_REC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_CHG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_CHG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_PROD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_FW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? LM_FW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_OW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? LM_OW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_PL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_PL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_PLC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_PLC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_OTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_OTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_OTHC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_OTHC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_STO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_STO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_GL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_GL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_GPROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_GPROD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_GLIFT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? LM_GLIFT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_CSIL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_CSIL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_JOHN), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_JOHN { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(LM_LTR_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? LM_LTR_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//01 MULTI-MAST REDEFINES FIELD-MAST.

		//02 MULTI-CODES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MULTI_W_REC_TYPE_REC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MULTI_W_REC_TYPE_REC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MULTI_W_REC_DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? MULTI_W_REC_DIST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MULTI_W_REC_FIELD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MULTI_W_REC_FIELD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MULTI_W_REC_OPR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MULTI_W_REC_OPR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MULTI_W_REC_LEASE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MULTI_W_REC_LEASE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MULTI_W_REC_OFFSHORE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MULTI_W_REC_OFFSHORE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 MULTI-DATA.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_RECORD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? M_RECORD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(TYPEW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? TYPEW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(RESER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? RESER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 UNIT-YATES REDEFINES RESER.

		//05 UNIT-NO-I PIC X.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? M_COUNTY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 MULTI-CO REDEFINES M-COUNTY.

		//05 M-CO-1 PIC 999.

		//05 M-CO-2 PIC 999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_TST_EFF), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? M_TST_EFF { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_PNTR_1ST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? M_PNTR_1ST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(CAP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? CAP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PROD_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PROD_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 PI-WELLS REDEFINES PROD-WELL.

		//05 M-PROD PIC 999.

		//05 M-INJ PIC 999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MARG_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MARG_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 SM-WELLS REDEFINES MARG-WELL.

		//05 M-SHUT PIC 999.

		//05 M-MARG PIC 999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? M_DEPTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_PNTR_LST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? M_PNTR_LST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_EXC_TEST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? M_EXC_TEST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_WATER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? M_WATER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_REMARK), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? M_REMARK { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_PRCNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? MM_PRCNT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 M-MONTH OCCURS 14 TIMES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(M_MONTH_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? M_MONTH_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 M-MONTH-DATE-REDF REDEFINES

		//07 M-MONTH-CCYY PIC 9(4).

		//07 M-MONTH-CCYY-REDF REDEFINES

		//09 MM-CC PIC 99.

		//09 MM-YR PIC 99.

		//07 MM-MO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_CHG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_CHG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_ACODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_ACODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_TCODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_TCODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_LIMIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_ALLOW2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_ALLOW2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_ACODE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_ACODE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_TCODE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_TCODE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_LIMIT2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_LIMIT2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_DATE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? MM_DATE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_ALLOW3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_ALLOW3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_ACODE3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_ACODE3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_TCODE3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_TCODE3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_LIMIT3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_LIMIT3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_DATE3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? MM_DATE3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_FORM_LCK), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_FORM_LCK { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_SPACE1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_SPACE1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_KODE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_KODE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_SPACE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? MM_SPACE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(MM_JOHN), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? MM_JOHN { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//01 WELL-MAST REDEFINES FIELD-MAST.

		//02 WELL-CODES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_REC_TYPE_REC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WELL_REC_TYPE_REC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_REC_DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? WELL_REC_DIST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_REC_FIELD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WELL_REC_FIELD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_REC_OPR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WELL_REC_OPR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_REC_LEASE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WELL_REC_LEASE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_REC_OFFSHORE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WELL_REC_OFFSHORE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 WELL-DATA.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WELL_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? WELL_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_TYPE_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? W_TYPE_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_UNIT_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? W_UNIT_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? W_KEY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? W_COUNTY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PUMP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? PUMP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 PUMPX REDEFINES PUMP PIC X.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_SP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_SP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_NET), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_NET { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 W-NGOR REDEFINES W-NET.

		//05 W-N-CODE PIC 9.

		//05 W-N-AMT PIC 9(5).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_DEPTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(SAND), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? SAND { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FROZEN), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? FROZEN { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(PERF), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? PERF { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 WELL-D REDEFINES W-DATE.

		//05 WELL-D-CCYY PIC 9(4).

		//05 WELL-D-CCYY-REDF REDEFINES

		//07 WELL-D-CC PIC 99.

		//07 WELL-D-YR PIC 99.

		//05 WELL-D-MO PIC 99.

		//05 WELL-D-DAY PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EX_14B_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? EX_14B_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_SUB_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? W_SUB_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 W-SUB-WELL-ALPHA REDEFINES W-SUB-WELL

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_NO_PROD_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? W_NO_PROD_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DELQ_FORM), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? W_DELQ_FORM { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_TST_EFF), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? W_TST_EFF { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_EXC_TST), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? W_EXC_TST { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_WATER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? W_WATER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(EX_14B_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? EX_14B_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 EX-14B-DATE-REDF REDEFINES EX-14B-DATE.

		//05 EX-CCYY-14B PIC 9(4).

		//05 EX-CCYY-14B-REDF REDEFINES

		//07 EX-CC-14B PIC 99.

		//07 EX-YR-14B PIC 99.

		//05 EX-MO-14B PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_RMKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? W_RMKS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(BONUS_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? BONUS_AMT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 BONS REDEFINES BONUS-AMT.

		//05 BONUS-CD PIC 9.

		//05 BONUS PIC 999.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(FROZTSF), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? FROZTSF { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_WLSD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? W_WLSD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_TST_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_TST_DT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 W-TEST-DATE REDEFINES W-TST-DT.

		//05 W-TST-CCYY PIC 9(4).

		//05 W-TST-CCYY-REDF REDEFINES W-TST-CCYY.

		//07 W-TST-CC PIC 99.

		//07 W-TST-YR PIC 99.

		//05 W-TST-MO PIC 99.

		//05 W-TST-DA PIC 99.

		//03 W-TST-DTE REDEFINES W-TST-DT.

		//05 W-T-DT-CC PIC 99.

		//05 W-T-DT PIC 9(4).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_DTE_LST_UTL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_DTE_LST_UTL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 W-DTE-LST-UTL-REDF REDEFINES W-DTE-LST-UTL.

		//05 W-DTE-LST-UTL-CCYY PIC 9(4).

		//05 W-DTE-LST-UTL-CCYY-REDF REDEFINES

		//07 W-DTE-LST-UTL-CC PIC 99.

		//07 W-DTE-LST-UTL-YY PIC 99.

		//05 W-DTE-LST-UTL-MM PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_NEW_WB_EXC), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? W_NEW_WB_EXC { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_NEW_WB_CONNECT_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_NEW_WB_CONNECT_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 W-NEW-WB-CONNECT-DATE-REDF REDEFINES

		//05 W-NEW-WB-CCYY PIC 9(4).

		//05 W-NEW-WB-CCYY-REDF REDEFINES

		//07 W-NEW-WB-CC PIC 99.

		//07 W-NEW-WB-YR PIC 99.

		//05 W-NEW-WB-MO PIC 99.

		//05 W-NEW-WB-DA PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_14B2_TYPE_COVERAGE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? W_14B2_TYPE_COVERAGE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_14B2_APP_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_14B2_APP_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 W-MONTH OCCURS 14 TIMES.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(W_MONTH_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? W_MONTH_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 W-MONTH-DATE-REDF REDEFINES W-MONTH-DATE.

		//07 WM-CCYY PIC 9(4).

		//07 WM-CCYY-REDF REDEFINES WM-CCYY.

		//09 WM-CC PIC 99.

		//09 WM-YR PIC 99.

		//07 WM-MO PIC 99.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_CHG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WM_CHG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WM_NO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_ALLOW { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ACODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? WM_ACODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_TCODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? WM_TCODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_LIMIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ALLOW2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_ALLOW2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ACODE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? WM_ACODE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_TCODE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? WM_TCODE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_LIMIT2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_LIMIT2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_DATE2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? WM_DATE2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ALLOW3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_ALLOW3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ACODE3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? WM_ACODE3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_TCODE3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? WM_TCODE3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_LIMIT3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_LIMIT3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_DATE3), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? WM_DATE3 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_FORM_LCK), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WM_FORM_LCK { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_PGT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? WM_PGT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_TSWA), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WM_TSWA { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_EGT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? WM_EGT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ESWA), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WM_ESWA { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ACRE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_ACRE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_POTE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_POTE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_ACFT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_ACFT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_GOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_GOR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_OTRAN_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WM_OTRAN_CD { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_POT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? WM_POT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_EOT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? WM_EOT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_JOHN), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public sbyte? WM_JOHN { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(WM_OOIP), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? WM_OOIP { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public OilLedger()
		{
		}

		public OilLedger(ReadOnlySpan<byte> source)
		{
			//02 OIL-CODES.
			TYPE_REC              = StringParser.ReadAsSByte(source, 1 - 1, 1);
			DIST              = StringParser.ReadAsString(source, 2 - 1, 3);
			FIELD              = StringParser.ReadAsInt64(source, 5 - 1, 8);
			OPR              = StringParser.ReadAsInt64(source, 13 - 1, 6);
			LEASE              = StringParser.ReadAsInt64(source, 19 - 1, 5);
			OFFSHORE              = StringParser.ReadAsSByte(source, 26 - 1, 1);
			//02 FIELD-DATA.
			F_NAME              = StringParser.ReadAsString(source, 27 - 1, 32);
			COUNTY              = StringParser.ReadAsInt32(source, 59 - 1, 3);
			//03 COUNTIES REDEFINES COUNTY
			//OCCURS 6 TIMES
			DISC_DATE              = StringParser.ReadAsInt64(source, 77 - 1, 8);
			//03 D-DATE REDEFINES DISC-DATE.
			//05 DSC-CCYY PIC 9(4).
			//05 DSC-CCYY-REDF REDEFINES DSC-CCYY.
			//07 DSC-CC PIC 99.
			//07 DSC-YR PIC 99.
			//05 DSC-MO PIC 99.
			//05 DSC-DAY PIC 99.
			F_DEPTH              = StringParser.ReadAsInt64(source, 85 - 1, 5);
			O_GRAV              = StringParser.ReadAsInt32(source, 90 - 1, 3);
			F_TYPE              = StringParser.ReadAsSByte(source, 93 - 1, 1);
			MULT_RES              = StringParser.ReadAsSByte(source, 94 - 1, 1);
			F_LPB              = StringParser.ReadAsSByte(source, 95 - 1, 1);
			F_XMT              = StringParser.ReadAsSByte(source, 96 - 1, 1);
			PRT_AS_IS              = StringParser.ReadAsSByte(source, 97 - 1, 1);
			YARD              = StringParser.ReadAsSByte(source, 98 - 1, 1);
			T_CODES              = StringParser.ReadAsInt64(source, 99 - 1, 12);
			//03 TEST-CODE REDEFINES T-CODES
			//OCCURS 12 TIMES
			ALLOCATION              = StringParser.ReadAsInt64(source, 111 - 1, 12);
			//03 FACTORS REDEFINES ALLOCATION
			//OCCURS 3 TIMES.
			//05 PERCNT PIC V99.
			//05 CODEF PIC 99.
			RES_AMT              = StringParser.ReadAsInt64(source, 123 - 1, 6);
			F_GOR              = StringParser.ReadAsInt64(source, 129 - 1, 6);
			//03 F-LIMIT REDEFINES F-GOR.
			//05 F-GOR-CODE PIC 9.
			//05 F-GOR-AMT PIC 9(5).
			F_TOP              = StringParser.ReadAsInt64(source, 135 - 1, 5);
			//03 F-MER REDEFINES F-TOP.
			//05 F-TOP-CODE PIC 9.
			//05 F-TOP-AMT PIC 9(4).
			F_NET              = StringParser.ReadAsInt64(source, 140 - 1, 6);
			//03 F-NGOR REDEFINES F-NET.
			//05 F-NET-CODE PIC 9.
			//05 F-NET-AMT PIC 9(5).
			UNET              = StringParser.ReadAsInt32(source, 146 - 1, 3);
			TOL              = StringParser.ReadAsInt32(source, 149 - 1, 4);
			//03 TOLER REDEFINES TOL.
			//05 F-TOL-CODE PIC 9.
			//05 F-TOL-AMT PIC 999.
			SPAC              = StringParser.ReadAsInt64(source, 153 - 1, 8);
			//03 SPAC1-2 REDEFINES SPAC.
			//05 SPAC1 PIC 9999.
			//05 SPAC2 PIC 9999.
			DIAG              = StringParser.ReadAsInt32(source, 161 - 1, 4);
			CUM_PROD              = StringParser.ReadAsPackedInt64(source, 165 - 1, 7);
			CASING              = StringParser.ReadAsString(source, 172 - 1, 21);
			COL_HEAD              = StringParser.ReadAsChar(source, 193 - 1, 1);
			ALO_CODE              = StringParser.ReadAsChar(source, 194 - 1, 1);
			F_RMK1              = StringParser.ReadAsString(source, 195 - 1, 66);
			F_RMK2              = StringParser.ReadAsString(source, 261 - 1, 66);
			PERM_NO              = StringParser.ReadAsString(source, 327 - 1, 5);
			SP_FHC              = StringParser.ReadAsSByte(source, 332 - 1, 1);
			//03 ANNUAL-1.
			AN_A              = StringParser.ReadAsString(source, 333 - 1, 90);
			AN_B              = StringParser.ReadAsString(source, 423 - 1, 35);
			//03 ANNUAL-2 REDEFINES ANNUAL-1.
			//05 AN-1 PIC X(66).
			//05 AN-2 PIC X(59).
			F_OOIP              = StringParser.ReadAsInt64(source, 458 - 1, 8);
			//03 F-MONTH OCCURS 14 TIMES.
			FM_DATE              = StringParser.ReadAsInt64(source, 501 - 1, 6);
			//05 FM-DATE-REDF REDEFINES FM-DATE.
			//07 FM-CCYY PIC 9(4).
			//07 FM-CCYY-REDF REDEFINES FM-CCYY.
			//09 FM-CC PIC 99.
			//09 FM-YR PIC 99.
			//07 FM-MO PIC 99.
			FM_PW              = StringParser.ReadAsPackedInt32(source, 507 - 1, 2);
			FM_AC              = StringParser.ReadAsPackedInt32(source, 509 - 1, 4);
			FM_OTHC              = StringParser.ReadAsSByte(source, 517 - 1, 1);
			FM_CHG              = StringParser.ReadAsSByte(source, 518 - 1, 1);
			FM_PROD_FACT              = StringParser.ReadAsPackedInt64(source, 519 - 1, 3);
			FM_SPLIT_PROD_FACT              = StringParser.ReadAsPackedInt64(source, 522 - 1, 3);
			//05 FM-SPLIT-PROD-FACT-DATE
			FM_JOHN              = StringParser.ReadAsSByte(source, 525 - 1, 1);
			FM_OTH              = StringParser.ReadAsPackedDouble(source, 7, 526 - 1, 8);
			//01 LEASE-MAST REDEFINES FIELD-MAST.
			//02 LSE-CODES.
			LEASE_REC_TYPE_REC              = StringParser.ReadAsSByte(source, 1 - 1, 1);
			LEASE_REC_DIST              = StringParser.ReadAsString(source, 2 - 1, 3);
			LEASE_REC_FIELD              = StringParser.ReadAsInt64(source, 5 - 1, 8);
			LEASE_REC_OPR              = StringParser.ReadAsInt64(source, 13 - 1, 6);
			LEASE_REC_LEASE              = StringParser.ReadAsInt64(source, 19 - 1, 5);
			LEASE_REC_OFFSHORE              = StringParser.ReadAsSByte(source, 26 - 1, 1);
			//02 LEASE-DATA.
			L_NAME              = StringParser.ReadAsString(source, 27 - 1, 32);
			LSE_CO              = StringParser.ReadAsInt64(source, 59 - 1, 6);
			//03 LEASE-CO REDEFINES LSE-CO.
			//05 L-CO-1 PIC 999.
			//05 L-CO-2 PIC 999.
			POGATH              = StringParser.ReadAsString(source, 65 - 1, 5);
			PGGATH              = StringParser.ReadAsString(source, 70 - 1, 5);
			OSPLIT              = StringParser.ReadAsSByte(source, 75 - 1, 1);
			GSPLIT              = StringParser.ReadAsSByte(source, 76 - 1, 1);
			OOGATH              = StringParser.ReadAsString(source, 77 - 1, 5);
			OGGATH              = StringParser.ReadAsString(source, 82 - 1, 5);
			OOPR              = StringParser.ReadAsInt64(source, 87 - 1, 6);
			BO_STATUS              = StringParser.ReadAsPackedInt64(source, 93 - 1, 4);
			BG_STATUS              = StringParser.ReadAsPackedInt64(source, 97 - 1, 4);
			MOVE_BAL              = StringParser.ReadAsPackedInt64(source, 101 - 1, 4);
			PO_STATUS              = StringParser.ReadAsPackedInt64(source, 105 - 1, 4);
			PG_STATUS              = StringParser.ReadAsPackedInt64(source, 109 - 1, 4);
			SEC_REC              = StringParser.ReadAsSByte(source, 113 - 1, 1);
			CERT              = StringParser.ReadAsInt16(source, 114 - 1, 2);
			BATCH              = StringParser.ReadAsChar(source, 116 - 1, 1);
			L_LPB              = StringParser.ReadAsSByte(source, 117 - 1, 1);
			COMMINGLE_CD              = StringParser.ReadAsSByte(source, 118 - 1, 1);
			COMMINGLE              = StringParser.ReadAsInt32(source, 119 - 1, 4);
			L_INFO              = StringParser.ReadAsString(source, 123 - 1, 54);
			AD_BO_STATUS              = StringParser.ReadAsPackedInt64(source, 177 - 1, 4);
			AD_BG_STATUS              = StringParser.ReadAsPackedInt64(source, 181 - 1, 4);
			COMMINGLE_DATE              = StringParser.ReadAsInt64(source, 185 - 1, 6);
			//03 COMMINGLE-DATE-REDF REDEFINES COMMINGLE-DATE.
			//05 COMMINGLE-CCYY PIC 9(4).
			//05 COMMINGLE-CCYY-REDF REDEFINES
			//07 COMMINGLE-CC PIC 99.
			//07 COMMINGLE-YR PIC 99.
			//05 COMMINGLE-MO PIC 99.
			L_RMCD              = StringParser.ReadAsSByte(source, 191 - 1, 1);
			L_RMDT              = StringParser.ReadAsInt64(source, 192 - 1, 6);
			//03 L-RMDT-REDF REDEFINES L-RMDT.
			//05 L-RMDT-CCYY PIC 9(4).
			//05 L-RMDT-CCYY-REDF REDEFINES L-RMDT-CCYY.
			//07 L-RMCC PIC 99.
			//07 L-RMYR PIC 99.
			//05 L-RMMO PIC 99.
			SEV_CD_13              = StringParser.ReadAsSByte(source, 198 - 1, 1);
			SEV_CD_14              = StringParser.ReadAsSByte(source, 199 - 1, 1);
			//03 CAS-RED.
			L_CAS_SI_LTR_DTE              = StringParser.ReadAsInt64(source, 200 - 1, 6);
			L_RED_RTE_DTE              = StringParser.ReadAsInt64(source, 206 - 1, 6);
			//03 CAS-RED-A REDEFINES CAS-RED.
			//05 L-CAS-CCYY PIC 9(4).
			//05 L-CAS-CCYY-REDF REDEFINES L-CAS-CCYY.
			//07 L-CAS-CC PIC 99.
			//07 L-CAS-YR PIC 99.
			//05 L-CAS-MO PIC 99.
			//05 L-RED-CCYY PIC 9(4).
			//05 L-RED-CCYY-REDF REDEFINES L-RED-CCYY.
			//07 L-RED-CC PIC 99.
			//07 L-RED-YR PIC 99.
			//05 L-RED-MO PIC 99.
			L_EXC_TST              = StringParser.ReadAsSByte(source, 212 - 1, 1);
			L_RLTYCD              = StringParser.ReadAsSByte(source, 213 - 1, 1);
			L_ONE_WELL_LEASE              = StringParser.ReadAsChar(source, 214 - 1, 1);
			L_PANHANDLE_GOR_EXC              = StringParser.ReadAsChar(source, 215 - 1, 6);
			//03 L-PANHANDLE-GOR-AMT PIC 9(08)V9 COMP-3. 216
			//03 L-MONTH OCCURS 12 TIMES.
			L_MONTH_DATE              = StringParser.ReadAsInt64(source, 225 - 1, 6);
			//05 L-MONTH-DATE-REDF REDEFINES
			//07 L-MONTH-CCYY PIC 9(4).
			//07 L-MONTH-CCYY-REDF REDEFINES
			//09 LM-CC PIC 99.
			//09 LM-YR PIC 99.
			//07 LM-MO PIC 99.
			LM_SEV              = StringParser.ReadAsSByte(source, 231 - 1, 1);
			LM_RETRO              = StringParser.ReadAsSByte(source, 232 - 1, 1);
			LM_REC              = StringParser.ReadAsSByte(source, 233 - 1, 1);
			LM_CHG              = StringParser.ReadAsSByte(source, 234 - 1, 1);
			LM_ALLOW              = StringParser.ReadAsPackedInt64(source, 235 - 1, 4);
			LM_PROD              = StringParser.ReadAsPackedInt64(source, 239 - 1, 4);
			LM_FW              = StringParser.ReadAsInt32(source, 243 - 1, 3);
			LM_OW              = StringParser.ReadAsInt32(source, 246 - 1, 3);
			LM_PL              = StringParser.ReadAsPackedInt64(source, 249 - 1, 4);
			LM_PLC              = StringParser.ReadAsSByte(source, 253 - 1, 1);
			LM_OTH              = StringParser.ReadAsPackedInt64(source, 254 - 1, 4);
			LM_OTHC              = StringParser.ReadAsSByte(source, 258 - 1, 1);
			LM_STO              = StringParser.ReadAsPackedInt64(source, 259 - 1, 4);
			LM_GL              = StringParser.ReadAsPackedInt64(source, 263 - 1, 5);
			LM_GPROD              = StringParser.ReadAsPackedInt64(source, 268 - 1, 5);
			LM_GLIFT              = StringParser.ReadAsPackedInt64(source, 273 - 1, 4);
			LM_CSIL              = StringParser.ReadAsSByte(source, 277 - 1, 1);
			LM_JOHN              = StringParser.ReadAsSByte(source, 278 - 1, 1);
			LM_LTR_CODE              = StringParser.ReadAsSByte(source, 279 - 1, 1);
			//01 MULTI-MAST REDEFINES FIELD-MAST.
			//02 MULTI-CODES.
			MULTI_W_REC_TYPE_REC              = StringParser.ReadAsSByte(source, 1 - 1, 1);
			MULTI_W_REC_DIST              = StringParser.ReadAsString(source, 2 - 1, 3);
			MULTI_W_REC_FIELD              = StringParser.ReadAsInt64(source, 5 - 1, 8);
			MULTI_W_REC_OPR              = StringParser.ReadAsInt64(source, 13 - 1, 6);
			MULTI_W_REC_LEASE              = StringParser.ReadAsInt64(source, 19 - 1, 5);
			MULTI_W_REC_OFFSHORE              = StringParser.ReadAsSByte(source, 26 - 1, 1);
			//02 MULTI-DATA.
			M_RECORD              = StringParser.ReadAsString(source, 27 - 1, 6);
			TYPEW              = StringParser.ReadAsChar(source, 33 - 1, 1);
			RESER              = StringParser.ReadAsString(source, 34 - 1, 5);
			//03 UNIT-YATES REDEFINES RESER.
			//05 UNIT-NO-I PIC X.
			M_COUNTY              = StringParser.ReadAsInt64(source, 39 - 1, 6);
			//03 MULTI-CO REDEFINES M-COUNTY.
			//05 M-CO-1 PIC 999.
			//05 M-CO-2 PIC 999.
			M_TST_EFF              = StringParser.ReadAsChar(source, 45 - 1, 1);
			M_PNTR_1ST              = StringParser.ReadAsInt64(source, 46 - 1, 6);
			CAP              = StringParser.ReadAsSByte(source, 52 - 1, 1);
			PROD_WELL              = StringParser.ReadAsInt64(source, 53 - 1, 6);
			//03 PI-WELLS REDEFINES PROD-WELL.
			//05 M-PROD PIC 999.
			//05 M-INJ PIC 999.
			MARG_WELL              = StringParser.ReadAsInt64(source, 59 - 1, 6);
			//03 SM-WELLS REDEFINES MARG-WELL.
			//05 M-SHUT PIC 999.
			//05 M-MARG PIC 999.
			M_DEPTH              = StringParser.ReadAsSByte(source, 65 - 1, 1);
			M_PNTR_LST              = StringParser.ReadAsInt64(source, 66 - 1, 6);
			M_EXC_TEST              = StringParser.ReadAsSByte(source, 72 - 1, 1);
			M_WATER              = StringParser.ReadAsInt64(source, 79 - 1, 6);
			M_REMARK              = StringParser.ReadAsString(source, 85 - 1, 55);
			MM_PRCNT              = StringParser.ReadAsInt32(source, 140 - 1, 3);
			//03 M-MONTH OCCURS 14 TIMES.
			M_MONTH_DATE              = StringParser.ReadAsInt64(source, 165 - 1, 6);
			//05 M-MONTH-DATE-REDF REDEFINES
			//07 M-MONTH-CCYY PIC 9(4).
			//07 M-MONTH-CCYY-REDF REDEFINES
			//09 MM-CC PIC 99.
			//09 MM-YR PIC 99.
			//07 MM-MO PIC 99.
			MM_CHG              = StringParser.ReadAsSByte(source, 171 - 1, 1);
			MM_NO              = StringParser.ReadAsSByte(source, 172 - 1, 1);
			MM_ALLOW              = StringParser.ReadAsPackedInt64(source, 173 - 1, 4);
			MM_ACODE              = StringParser.ReadAsSByte(source, 177 - 1, 1);
			MM_TCODE              = StringParser.ReadAsSByte(source, 178 - 1, 1);
			MM_LIMIT              = StringParser.ReadAsPackedInt64(source, 179 - 1, 5);
			MM_ALLOW2              = StringParser.ReadAsPackedInt64(source, 184 - 1, 4);
			MM_ACODE2              = StringParser.ReadAsSByte(source, 188 - 1, 1);
			MM_TCODE2              = StringParser.ReadAsSByte(source, 189 - 1, 1);
			MM_LIMIT2              = StringParser.ReadAsPackedInt64(source, 190 - 1, 5);
			MM_DATE2              = StringParser.ReadAsInt16(source, 195 - 1, 2);
			MM_ALLOW3              = StringParser.ReadAsPackedInt64(source, 197 - 1, 4);
			MM_ACODE3              = StringParser.ReadAsSByte(source, 201 - 1, 1);
			MM_TCODE3              = StringParser.ReadAsSByte(source, 202 - 1, 1);
			MM_LIMIT3              = StringParser.ReadAsPackedInt64(source, 203 - 1, 5);
			MM_DATE3              = StringParser.ReadAsInt16(source, 208 - 1, 2);
			MM_FORM_LCK              = StringParser.ReadAsSByte(source, 210 - 1, 1);
			MM_SPACE1              = StringParser.ReadAsPackedInt64(source, 211 - 1, 4);
			MM_KODE2              = StringParser.ReadAsSByte(source, 215 - 1, 1);
			MM_SPACE2              = StringParser.ReadAsPackedInt64(source, 216 - 1, 4);
			MM_JOHN              = StringParser.ReadAsSByte(source, 220 - 1, 1);
			//01 WELL-MAST REDEFINES FIELD-MAST.
			//02 WELL-CODES.
			WELL_REC_TYPE_REC              = StringParser.ReadAsSByte(source, 1 - 1, 1);
			WELL_REC_DIST              = StringParser.ReadAsString(source, 2 - 1, 3);
			WELL_REC_FIELD              = StringParser.ReadAsInt64(source, 5 - 1, 8);
			WELL_REC_OPR              = StringParser.ReadAsInt64(source, 13 - 1, 6);
			WELL_REC_LEASE              = StringParser.ReadAsInt64(source, 19 - 1, 5);
			WELL_REC_OFFSHORE              = StringParser.ReadAsSByte(source, 26 - 1, 1);
			//02 WELL-DATA.
			WELL_NO              = StringParser.ReadAsString(source, 27 - 1, 6);
			W_TYPE_WELL              = StringParser.ReadAsChar(source, 33 - 1, 1);
			W_UNIT_NO              = StringParser.ReadAsChar(source, 34 - 1, 1);
			W_KEY              = StringParser.ReadAsSByte(source, 39 - 1, 1);
			W_COUNTY              = StringParser.ReadAsInt32(source, 40 - 1, 3);
			PUMP              = StringParser.ReadAsSByte(source, 43 - 1, 1);
			//03 PUMPX REDEFINES PUMP PIC X.
			W_SP              = StringParser.ReadAsInt64(source, 44 - 1, 5);
			W_NET              = StringParser.ReadAsInt64(source, 49 - 1, 6);
			//03 W-NGOR REDEFINES W-NET.
			//05 W-N-CODE PIC 9.
			//05 W-N-AMT PIC 9(5).
			W_DEPTH              = StringParser.ReadAsInt64(source, 55 - 1, 5);
			SAND              = StringParser.ReadAsInt32(source, 60 - 1, 3);
			FROZEN              = StringParser.ReadAsInt64(source, 63 - 1, 5);
			PERF              = StringParser.ReadAsInt64(source, 68 - 1, 5);
			W_DATE              = StringParser.ReadAsInt64(source, 73 - 1, 8);
			//03 WELL-D REDEFINES W-DATE.
			//05 WELL-D-CCYY PIC 9(4).
			//05 WELL-D-CCYY-REDF REDEFINES
			//07 WELL-D-CC PIC 99.
			//07 WELL-D-YR PIC 99.
			//05 WELL-D-MO PIC 99.
			//05 WELL-D-DAY PIC 99.
			EX_14B_CD              = StringParser.ReadAsChar(source, 81 - 1, 1);
			W_SUB_WELL              = StringParser.ReadAsSByte(source, 82 - 1, 1);
			//03 W-SUB-WELL-ALPHA REDEFINES W-SUB-WELL
			W_NO_PROD_CD              = StringParser.ReadAsSByte(source, 83 - 1, 1);
			W_DELQ_FORM              = StringParser.ReadAsSByte(source, 84 - 1, 1);
			W_TST_EFF              = StringParser.ReadAsChar(source, 85 - 1, 1);
			W_EXC_TST              = StringParser.ReadAsSByte(source, 86 - 1, 1);
			W_WATER              = StringParser.ReadAsInt32(source, 87 - 1, 4);
			EX_14B_DATE              = StringParser.ReadAsInt64(source, 91 - 1, 6);
			//03 EX-14B-DATE-REDF REDEFINES EX-14B-DATE.
			//05 EX-CCYY-14B PIC 9(4).
			//05 EX-CCYY-14B-REDF REDEFINES
			//07 EX-CC-14B PIC 99.
			//07 EX-YR-14B PIC 99.
			//05 EX-MO-14B PIC 99.
			W_RMKS              = StringParser.ReadAsString(source, 97 - 1, 15);
			BONUS_AMT              = StringParser.ReadAsInt32(source, 112 - 1, 4);
			//03 BONS REDEFINES BONUS-AMT.
			//05 BONUS-CD PIC 9.
			//05 BONUS PIC 999.
			FROZTSF              = StringParser.ReadAsInt32(source, 116 - 1, 3);
			W_WLSD              = StringParser.ReadAsSByte(source, 119 - 1, 1);
			W_TST_DT              = StringParser.ReadAsInt64(source, 120 - 1, 8);
			//03 W-TEST-DATE REDEFINES W-TST-DT.
			//05 W-TST-CCYY PIC 9(4).
			//05 W-TST-CCYY-REDF REDEFINES W-TST-CCYY.
			//07 W-TST-CC PIC 99.
			//07 W-TST-YR PIC 99.
			//05 W-TST-MO PIC 99.
			//05 W-TST-DA PIC 99.
			//03 W-TST-DTE REDEFINES W-TST-DT.
			//05 W-T-DT-CC PIC 99.
			//05 W-T-DT PIC 9(4).
			W_DTE_LST_UTL              = StringParser.ReadAsInt64(source, 128 - 1, 6);
			//03 W-DTE-LST-UTL-REDF REDEFINES W-DTE-LST-UTL.
			//05 W-DTE-LST-UTL-CCYY PIC 9(4).
			//05 W-DTE-LST-UTL-CCYY-REDF REDEFINES
			//07 W-DTE-LST-UTL-CC PIC 99.
			//07 W-DTE-LST-UTL-YY PIC 99.
			//05 W-DTE-LST-UTL-MM PIC 99.
			W_NEW_WB_EXC              = StringParser.ReadAsChar(source, 134 - 1, 1);
			W_NEW_WB_CONNECT_DATE              = StringParser.ReadAsInt64(source, 135 - 1, 8);
			//03 W-NEW-WB-CONNECT-DATE-REDF REDEFINES
			//05 W-NEW-WB-CCYY PIC 9(4).
			//05 W-NEW-WB-CCYY-REDF REDEFINES
			//07 W-NEW-WB-CC PIC 99.
			//07 W-NEW-WB-YR PIC 99.
			//05 W-NEW-WB-MO PIC 99.
			//05 W-NEW-WB-DA PIC 99.
			W_14B2_TYPE_COVERAGE              = StringParser.ReadAsChar(source, 143 - 1, 1);
			W_14B2_APP_NO              = StringParser.ReadAsInt64(source, 144 - 1, 6);
			//03 W-MONTH OCCURS 14 TIMES.
			W_MONTH_DATE              = StringParser.ReadAsInt64(source, 179 - 1, 6);
			//05 W-MONTH-DATE-REDF REDEFINES W-MONTH-DATE.
			//07 WM-CCYY PIC 9(4).
			//07 WM-CCYY-REDF REDEFINES WM-CCYY.
			//09 WM-CC PIC 99.
			//09 WM-YR PIC 99.
			//07 WM-MO PIC 99.
			WM_CHG              = StringParser.ReadAsSByte(source, 185 - 1, 1);
			WM_NO              = StringParser.ReadAsSByte(source, 186 - 1, 1);
			WM_ALLOW              = StringParser.ReadAsPackedInt64(source, 187 - 1, 3);
			WM_ACODE              = StringParser.ReadAsChar(source, 190 - 1, 1);
			WM_TCODE              = StringParser.ReadAsChar(source, 191 - 1, 1);
			WM_LIMIT              = StringParser.ReadAsPackedInt64(source, 192 - 1, 4);
			WM_ALLOW2              = StringParser.ReadAsPackedInt64(source, 196 - 1, 3);
			WM_ACODE2              = StringParser.ReadAsChar(source, 199 - 1, 1);
			WM_TCODE2              = StringParser.ReadAsChar(source, 200 - 1, 1);
			WM_LIMIT2              = StringParser.ReadAsPackedInt64(source, 201 - 1, 4);
			WM_DATE2              = StringParser.ReadAsInt16(source, 205 - 1, 2);
			WM_ALLOW3              = StringParser.ReadAsPackedInt64(source, 207 - 1, 3);
			WM_ACODE3              = StringParser.ReadAsChar(source, 210 - 1, 1);
			WM_TCODE3              = StringParser.ReadAsChar(source, 211 - 1, 1);
			WM_LIMIT3              = StringParser.ReadAsPackedInt64(source, 212 - 1, 4);
			WM_DATE3              = StringParser.ReadAsInt16(source, 216 - 1, 2);
			WM_FORM_LCK              = StringParser.ReadAsSByte(source, 218 - 1, 1);
			WM_PGT              = StringParser.ReadAsPackedInt32(source, 219 - 1, 2);
			WM_TSWA              = StringParser.ReadAsSByte(source, 221 - 1, 1);
			WM_EGT              = StringParser.ReadAsPackedInt32(source, 222 - 1, 2);
			WM_ESWA              = StringParser.ReadAsSByte(source, 224 - 1, 1);
			WM_ACRE              = StringParser.ReadAsPackedInt64(source, 225 - 1, 3);
			WM_POTE              = StringParser.ReadAsPackedInt64(source, 228 - 1, 3);
			WM_ACFT              = StringParser.ReadAsPackedInt64(source, 231 - 1, 3);
			WM_GOR              = StringParser.ReadAsPackedInt64(source, 234 - 1, 3);
			WM_OTRAN_CD              = StringParser.ReadAsSByte(source, 237 - 1, 1);
			WM_POT              = StringParser.ReadAsPackedInt32(source, 238 - 1, 2);
			WM_EOT              = StringParser.ReadAsPackedInt32(source, 240 - 1, 2);
			WM_JOHN              = StringParser.ReadAsSByte(source, 242 - 1, 1);
			WM_OOIP              = StringParser.ReadAsInt64(source, 243 - 1, 6);
		}
	}
}
