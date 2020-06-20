// ReSharper disable InconsistentNaming

using System;

namespace OilGas.Data.RRC.Texas
{
    public sealed class DistrictCode : IEquatable<DistrictCode>
    {
        public enum Kind
        {
            None_Selected,
            One,
            Two,
            Three,
            Four,
            Five,
            Six,
            SixE,
            SevenB,
            SevenC,
            Eight,
            EightA,
            Nine,
            Ten
        }

        public Kind Value { get; set; }

        public DistrictCode()
        {
            Value = Kind.None_Selected;
        }

        public DistrictCode(Kind value)
        {
            Value = value;
        }

        public DistrictCode(string value)
        {
            Value = FromName(value);
        }

        public static implicit operator int(DistrictCode districtCode)
        {
            return (int)districtCode.Value;
        }

        public static implicit operator string(DistrictCode districtCode)
        {
            return districtCode.ToString();
        }

        public override string ToString()
        {
            return GetName(Value);
        }

        private static string GetName(Kind kind)
        {
            switch(kind)
            {
                case Kind.None_Selected: return "None Selected";
                case Kind.One:           return "01";
                case Kind.Two:           return "02";
                case Kind.Three:         return "03";
                case Kind.Four:          return "04";
                case Kind.Five:          return "05";
                case Kind.Six:           return "06";
                case Kind.SixE:          return "6E";
                case Kind.SevenB:        return "7B";
                case Kind.SevenC:        return "7C";
                case Kind.Eight:         return "08";
                case Kind.EightA:        return "8A";
                case Kind.Nine:          return "09";
                case Kind.Ten:           return "10";
                default:
                    throw new ArgumentOutOfRangeException(nameof(kind),
                                                          kind,
                                                          null);
            }
        }

        private static Kind FromName(string kind)
        {
            switch(kind.Trim())
            {
                case "None Selected": return Kind.None_Selected;
                case "01":            return Kind.One;
                case "02":            return Kind.Two;
                case "03":            return Kind.Three;
                case "04":            return Kind.Four;
                case "05":            return Kind.Five;
                case "06":            return Kind.Six;
                case "6E":            return Kind.SixE;
                case "7B":            return Kind.SevenB;
                case "7C":            return Kind.SevenC;
                case "08":            return Kind.Eight;
                case "8A":            return Kind.EightA;
                case "09":            return Kind.Nine;
                case "10":            return Kind.Ten;

                default:
                    throw new ArgumentOutOfRangeException(nameof(kind),
                                                          kind,
                                                          null);
            }
        }

        public bool Equals(DistrictCode other)
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
                   obj is DistrictCode other && Equals(other);
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public static bool operator ==(DistrictCode left,
                                       DistrictCode right)
        {
            return Equals(left,
                          right);
        }

        public static bool operator !=(DistrictCode left,
                                       DistrictCode right)
        {
            return !Equals(left,
                           right);
        }

        public static bool operator ==(Kind         left,
                                       DistrictCode right)
        {
            return right != null && left == right.Value;
        }

        public static bool operator !=(Kind         left,
                                       DistrictCode right)
        {
            return !(left == right);
        }

        public static bool operator ==(DistrictCode left,
                                       Kind         right)
        {
            return left != null && left.Value == right;
        }

        public static bool operator !=(DistrictCode left,
                                       Kind         right)
        {
            return !(left == right);
        }
    }
}