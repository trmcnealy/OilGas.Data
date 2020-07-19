// ReSharper disable InconsistentNaming

using System;
using System.Runtime.CompilerServices;

namespace OilGas.Data.RRC.Texas
{
    public sealed class LeaseType : IEquatable<LeaseType>
    {
        public enum Kind
        {
            Oil,
            Gas,
            Both
        }

        private static readonly LeaseType OilType = new LeaseType("O");

        public static LeaseType Oil
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get { return OilType; }
        }

        private static readonly LeaseType GasType = new LeaseType("G");

        public static LeaseType Gas
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get { return GasType; }
        }

        private static readonly LeaseType BothType = new LeaseType("");

        public static LeaseType Both
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get { return BothType; }
        }

        public Kind Value { get; set; }

        public LeaseType()
        {
            Value = Kind.Oil;
        }

        public LeaseType(Kind value)
        {
            Value = value;
        }

        public LeaseType(string value)
        {
            Value = FromName(value);
        }

        public static implicit operator int(LeaseType leaseType)
        {
            return (int)leaseType.Value;
        }

        //public static implicit operator string(LeaseType leaseType)
        //{
        //    return leaseType.ToString();
        //}

        public override string ToString()
        {
            return GetName(Value);
        }

        private static string GetName(Kind kind)
        {
            switch(kind)
            {
                case Kind.Oil:  return "O";
                case Kind.Gas:  return "G";
                case Kind.Both: return "";
                default:
                    throw new ArgumentOutOfRangeException(nameof(kind),
                                                          kind,
                                                          null);
            }
        }

        private static Kind FromName(string kind)
        {
            switch(kind)
            {
                case "O": return Kind.Oil;
                case "G": return Kind.Gas;
                case "":  return Kind.Both;

                default:
                    throw new ArgumentOutOfRangeException(nameof(kind),
                                                          kind,
                                                          null);
            }
        }

        public bool Equals(LeaseType other)
        {
            if(ReferenceEquals(null,
                               other))
            {
                return false;
            }

            if(ReferenceEquals(this,
                               other))
            {
                return true;
            }

            return Value == other.Value;
        }

        public bool Equals(Kind other)
        {
            return Value == other;
        }

        public override bool Equals(object obj)
        {
            return ReferenceEquals(this,
                                   obj) ||
                   obj is LeaseType other && Equals(other);
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public static bool operator ==(LeaseType left,
                                       LeaseType right)
        {
            return Equals(left,
                          right);
        }

        public static bool operator !=(LeaseType left,
                                       LeaseType right)
        {
            return !Equals(left,
                           right);
        }

        public static bool operator ==(Kind      left,
                                       LeaseType right)
        {
            return right != null && left == right.Value;
        }

        public static bool operator !=(Kind      left,
                                       LeaseType right)
        {
            return !(left == right);
        }

        public static bool operator ==(LeaseType left,
                                       Kind      right)
        {
            return left != null && left.Value == right;
        }

        public static bool operator !=(LeaseType left,
                                       Kind      right)
        {
            return !(left == right);
        }
    }
}