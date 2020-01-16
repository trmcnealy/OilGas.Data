// ReSharper disable ConvertToAutoProperty
// ReSharper disable InconsistentNaming

using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace OilGas.Data.RRC.Texas
{
    [System.Runtime.Versioning.NonVersionable]
    public sealed class WellType : IEnumerable<WellType>
    {
        private readonly string _value;
        public string Value { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return _value; } }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private WellType(string value) { _value = value; }

        private static readonly WellType None_SelectedType = new WellType("None Selected");
        public static WellType None_Selected { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return None_SelectedType; } }

        private static readonly WellType ABANDONEDType = new WellType("AB");
        public static WellType ABANDONED { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return ABANDONEDType; } }

        private static readonly WellType BRINE_MININGType = new WellType("BM");
        public static WellType BRINE_MINING { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return BRINE_MININGType; } }

        private static readonly WellType DOMESTIC_USE_WELLType = new WellType("DW");
        public static WellType DOMESTIC_USE_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return DOMESTIC_USE_WELLType; } }

        private static readonly WellType GAS_STRG_INJECTIONType = new WellType("GJ");
        public static WellType GAS_STRG_INJECTION { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return GAS_STRG_INJECTIONType; } }

        private static readonly WellType GAS_STRG_SALT_FORMATIONType = new WellType("GL");
        public static WellType GAS_STRG_SALT_FORMATION { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return GAS_STRG_SALT_FORMATIONType; } }

        private static readonly WellType GEOTHERMAL_WELLType = new WellType("GT");
        public static WellType GEOTHERMAL_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return GEOTHERMAL_WELLType; } }

        private static readonly WellType GAS_STRG_WITHDRAWALType = new WellType("GW");
        public static WellType GAS_STRG_WITHDRAWAL { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return GAS_STRG_WITHDRAWALType; } }

        private static readonly WellType HISTORYType = new WellType("HI");
        public static WellType HISTORY { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return HISTORYType; } }

        private static readonly WellType INJECTIONType = new WellType("IN");
        public static WellType INJECTION { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return INJECTIONType; } }

        private static readonly WellType LPG_STORAGEType = new WellType("LP");
        public static WellType LPG_STORAGE { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return LPG_STORAGEType; } }

        private static readonly WellType LEASE_USE_WELLType = new WellType("LU");
        public static WellType LEASE_USE_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return LEASE_USE_WELLType; } }

        private static readonly WellType NO_PRODUCTIONType = new WellType("NP");
        public static WellType NO_PRODUCTION { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return NO_PRODUCTIONType; } }

        private static readonly WellType OBSERVATIONType = new WellType("OB");
        public static WellType OBSERVATION { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return OBSERVATIONType; } }

        private static readonly WellType OTHER_TYPE_SERVICEType = new WellType("OS");
        public static WellType OTHER_TYPE_SERVICE { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return OTHER_TYPE_SERVICEType; } }

        private static readonly WellType PROD_FACTOR_WELLType = new WellType("PF");
        public static WellType PROD_FACTOR_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return PROD_FACTOR_WELLType; } }

        private static readonly WellType PARTIAL_PLUGType = new WellType("PP");
        public static WellType PARTIAL_PLUG { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return PARTIAL_PLUGType; } }

        private static readonly WellType PRODUCINGType = new WellType("PR");
        public static WellType PRODUCING { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return PRODUCINGType; } }

        private static readonly WellType SWR_10_WELLType = new WellType("RT");
        public static WellType SWR_10_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return SWR_10_WELLType; } }

        private static readonly WellType SEALEDType = new WellType("SD");
        public static WellType SEALED { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return SEALEDType; } }

        private static readonly WellType SHUT_INType = new WellType("SH");
        public static WellType SHUT_IN { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return SHUT_INType; } }

        private static readonly WellType SHUT_IN_MULTI_COMPLType = new WellType("SM");
        public static WellType SHUT_IN_MULTI_COMPL { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return SHUT_IN_MULTI_COMPLType; } }

        private static readonly WellType TEMP_ABANDONEDType = new WellType("TA");
        public static WellType TEMP_ABANDONED { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return TEMP_ABANDONEDType; } }

        private static readonly WellType TRAININGType = new WellType("TR");
        public static WellType TRAINING { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return TRAININGType; } }

        private static readonly WellType WATER_SUPPLYType = new WellType("WS");
        public static WellType WATER_SUPPLY { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return WATER_SUPPLYType; } }

        private static readonly WellType NOT_ELIGIBLE_FOR_ALLOWABLEType = new WellType("ZZ");
        public static WellType NOT_ELIGIBLE_FOR_ALLOWABLE { [MethodImpl(MethodImplOptions.AggressiveInlining)] get { return NOT_ELIGIBLE_FOR_ALLOWABLEType; } }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public IEnumerator<WellType> GetEnumerator()
        {
            yield return None_Selected;
            yield return ABANDONED;
            yield return BRINE_MINING;
            yield return DOMESTIC_USE_WELL;
            yield return GAS_STRG_INJECTION;
            yield return GAS_STRG_SALT_FORMATION;
            yield return GEOTHERMAL_WELL;
            yield return GAS_STRG_WITHDRAWAL;
            yield return HISTORY;
            yield return INJECTION;
            yield return LPG_STORAGE;
            yield return LEASE_USE_WELL;
            yield return NO_PRODUCTION;
            yield return OBSERVATION;
            yield return OTHER_TYPE_SERVICE;
            yield return PROD_FACTOR_WELL;
            yield return PARTIAL_PLUG;
            yield return PRODUCING;
            yield return SWR_10_WELL;
            yield return SEALED;
            yield return SHUT_IN;
            yield return SHUT_IN_MULTI_COMPL;
            yield return TEMP_ABANDONED;
            yield return TRAINING;
            yield return WATER_SUPPLY;
            yield return NOT_ELIGIBLE_FOR_ALLOWABLE;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override string ToString()
        {
            return _value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static implicit operator string(WellType welltype)
        {
            return welltype.Value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static implicit operator WellType(string value)
        {
            return new WellType(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Equals(WellType other) { if(ReferenceEquals(null, other)) { return false; } if(ReferenceEquals(this, other)) { return true; } return Value == other.Value; }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override bool Equals(object obj) { if(ReferenceEquals(null, obj)) { return false; } if(ReferenceEquals(this, obj)) { return true; } return obj is WellType kind && Equals(kind); }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override int GetHashCode() { return Value.GetHashCode(); }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator ==(WellType left, WellType right) { return Equals(left, right); }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator ==(WellType left, string right) { return left?.Value == right; }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator ==(string left, WellType right) { return left == right?.Value; }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator !=(WellType left, WellType right) { return !Equals(left, right); }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator !=(WellType left, string right) { return left?.Value != right; }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator !=(string left, WellType right) { return left != right?.Value; }

    }

}

