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
        public ref readonly string Value { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref _value; } }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private WellType(string value) { _value = value; }

        private static readonly WellType None_SelectedType = new WellType("None Selected");
        public static ref readonly WellType None_Selected { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref None_SelectedType; } }

        private static readonly WellType ABANDONEDType = new WellType("AB");
        public static ref readonly WellType ABANDONED { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref ABANDONEDType; } }

        private static readonly WellType BRINE_MININGType = new WellType("BM");
        public static ref readonly WellType BRINE_MINING { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref BRINE_MININGType; } }

        private static readonly WellType DOMESTIC_USE_WELLType = new WellType("DW");
        public static ref readonly WellType DOMESTIC_USE_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref DOMESTIC_USE_WELLType; } }

        private static readonly WellType GAS_STRG_INJECTIONType = new WellType("GJ");
        public static ref readonly WellType GAS_STRG_INJECTION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref GAS_STRG_INJECTIONType; } }

        private static readonly WellType GAS_STRG_SALT_FORMATIONType = new WellType("GL");
        public static ref readonly WellType GAS_STRG_SALT_FORMATION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref GAS_STRG_SALT_FORMATIONType; } }

        private static readonly WellType GEOTHERMAL_WELLType = new WellType("GT");
        public static ref readonly WellType GEOTHERMAL_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref GEOTHERMAL_WELLType; } }

        private static readonly WellType GAS_STRG_WITHDRAWALType = new WellType("GW");
        public static ref readonly WellType GAS_STRG_WITHDRAWAL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref GAS_STRG_WITHDRAWALType; } }

        private static readonly WellType HISTORYType = new WellType("HI");
        public static ref readonly WellType HISTORY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref HISTORYType; } }

        private static readonly WellType INJECTIONType = new WellType("IN");
        public static ref readonly WellType INJECTION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref INJECTIONType; } }

        private static readonly WellType LPG_STORAGEType = new WellType("LP");
        public static ref readonly WellType LPG_STORAGE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref LPG_STORAGEType; } }

        private static readonly WellType LEASE_USE_WELLType = new WellType("LU");
        public static ref readonly WellType LEASE_USE_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref LEASE_USE_WELLType; } }

        private static readonly WellType NO_PRODUCTIONType = new WellType("NP");
        public static ref readonly WellType NO_PRODUCTION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref NO_PRODUCTIONType; } }

        private static readonly WellType OBSERVATIONType = new WellType("OB");
        public static ref readonly WellType OBSERVATION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref OBSERVATIONType; } }

        private static readonly WellType OTHER_TYPE_SERVICEType = new WellType("OS");
        public static ref readonly WellType OTHER_TYPE_SERVICE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref OTHER_TYPE_SERVICEType; } }

        private static readonly WellType PROD_FACTOR_WELLType = new WellType("PF");
        public static ref readonly WellType PROD_FACTOR_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref PROD_FACTOR_WELLType; } }

        private static readonly WellType PARTIAL_PLUGType = new WellType("PP");
        public static ref readonly WellType PARTIAL_PLUG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref PARTIAL_PLUGType; } }

        private static readonly WellType PRODUCINGType = new WellType("PR");
        public static ref readonly WellType PRODUCING { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref PRODUCINGType; } }

        private static readonly WellType SWR_10_WELLType = new WellType("RT");
        public static ref readonly WellType SWR_10_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref SWR_10_WELLType; } }

        private static readonly WellType SEALEDType = new WellType("SD");
        public static ref readonly WellType SEALED { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref SEALEDType; } }

        private static readonly WellType SHUT_INType = new WellType("SH");
        public static ref readonly WellType SHUT_IN { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref SHUT_INType; } }

        private static readonly WellType SHUT_IN_MULTI_COMPLType = new WellType("SM");
        public static ref readonly WellType SHUT_IN_MULTI_COMPL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref SHUT_IN_MULTI_COMPLType; } }

        private static readonly WellType TEMP_ABANDONEDType = new WellType("TA");
        public static ref readonly WellType TEMP_ABANDONED { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref TEMP_ABANDONEDType; } }

        private static readonly WellType TRAININGType = new WellType("TR");
        public static ref readonly WellType TRAINING { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref TRAININGType; } }

        private static readonly WellType WATER_SUPPLYType = new WellType("WS");
        public static ref readonly WellType WATER_SUPPLY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref WATER_SUPPLYType; } }

        private static readonly WellType NOT_ELIGIBLE_FOR_ALLOWABLEType = new WellType("ZZ");
        public static ref readonly WellType NOT_ELIGIBLE_FOR_ALLOWABLE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref NOT_ELIGIBLE_FOR_ALLOWABLEType; } }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
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

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public override string ToString()
        {
            return _value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static implicit operator string(WellType welltype)
        {
            return welltype._value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static implicit operator WellType(string value)
        {
            return new WellType(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public bool Equals(WellType other) { return _value == other._value; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public override bool Equals(object obj) { return obj is WellType kind && Equals(kind); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public override int GetHashCode() { return _value.GetHashCode(); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator ==(WellType left, WellType right) { return Equals(left, right); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator ==(WellType left, string right) { return left?._value == right; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator ==(string left, WellType right) { return left == right?._value; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator !=(WellType left, WellType right) { return !Equals(left, right); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator !=(WellType left, string right) { return left?._value != right; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator !=(string left, WellType right) { return left != right?._value; }

    }

}

