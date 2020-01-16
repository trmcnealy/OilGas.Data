// ReSharper disable InconsistentNaming

using System;

namespace OilGas.Data.RRC.Texas
{
    public sealed class MonthType : IEquatable<MonthType>
    {
        public enum Kind
        {
            Jan = 1,
            Feb,
            Mar,
            Apr,
            May,
            Jun,
            Jul,
            Aug,
            Sep,
            Oct,
            Nov,
            Dec
        }

        public Kind Value { get; set; }

        public MonthType()
        {
            Value = Kind.Jan;
        }

        public MonthType(Kind value)
        {
            Value = value;
        }

        public MonthType(string value)
        {
            Value = FromName(value);
        }

        public static implicit operator int(MonthType districtCode)
        {
            return (int)districtCode.Value;
        }

        public static implicit operator string(MonthType districtCode)
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
                case Kind.Jan: return "01";
                case Kind.Feb: return "02";
                case Kind.Mar: return "03";
                case Kind.Apr: return "04";
                case Kind.May: return "05";
                case Kind.Jun: return "06";
                case Kind.Jul: return "07";
                case Kind.Aug: return "08";
                case Kind.Sep: return "09";
                case Kind.Oct: return "10";
                case Kind.Nov: return "11";
                case Kind.Dec: return "12";
                default:
                    throw new ArgumentOutOfRangeException(nameof(kind),
                                                          kind,
                                                          null);
            }
        }

        internal static Kind FromName(string kind)
        {
            switch(kind)
            {
                case "01": return Kind.Jan;
                case "02": return Kind.Feb;
                case "03": return Kind.Mar;
                case "04": return Kind.Apr;
                case "05": return Kind.May;
                case "06": return Kind.Jun;
                case "07": return Kind.Jul;
                case "08": return Kind.Aug;
                case "09": return Kind.Sep;
                case "10": return Kind.Oct;
                case "11": return Kind.Nov;
                case "12": return Kind.Dec;
                default:
                    throw new ArgumentOutOfRangeException(nameof(kind),
                                                          kind,
                                                          null);
            }
        }

        internal static Kind FromMonthName(string kind)
        {
            switch(kind)
            {
                case "Jan": return Kind.Jan;
                case "Feb": return Kind.Feb;
                case "Mar": return Kind.Mar;
                case "Apr": return Kind.Apr;
                case "May": return Kind.May;
                case "Jun": return Kind.Jun;
                case "Jul": return Kind.Jul;
                case "Aug": return Kind.Aug;
                case "Sep": return Kind.Sep;
                case "Oct": return Kind.Oct;
                case "Nov": return Kind.Nov;
                case "Dec": return Kind.Dec;
                default:
                    throw new ArgumentOutOfRangeException(nameof(kind),
                                                          kind,
                                                          null);
            }
        }

        public bool Equals(MonthType other)
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
                   obj is MonthType other && Equals(other);
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public static bool operator ==(MonthType left,
                                       MonthType right)
        {
            return Equals(left,
                          right);
        }

        public static bool operator !=(MonthType left,
                                       MonthType right)
        {
            return !Equals(left,
                           right);
        }

        public static bool operator ==(Kind      left,
                                       MonthType right)
        {
            return right != null && left == right.Value;
        }

        public static bool operator !=(Kind      left,
                                       MonthType right)
        {
            return !(left == right);
        }

        public static bool operator ==(MonthType left,
                                       Kind      right)
        {
            return left != null && left.Value == right;
        }

        public static bool operator !=(MonthType left,
                                       Kind      right)
        {
            return !(left == right);
        }
    }
}