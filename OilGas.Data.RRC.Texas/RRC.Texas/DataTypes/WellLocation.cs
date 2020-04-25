using System;
using System.Collections.Generic;

namespace OilGas.Data.RRC.Texas
{
    public class WellLocation : IEquatable<WellLocation>, IComparable<WellLocation>, IComparable
    {
        public double Latitude { get; }

        public double Longitude { get; }

        public string Projection { get; }

        public WellLocation(double latitude,
                            double longitude,
                            string projection)
        {
            Latitude   = latitude;
            Longitude  = longitude;
            Projection = projection;
        }

        public override string ToString()
        {
            return $"({Latitude}, {Longitude})";
        }

        public bool Equals(WellLocation other)
        {
            if(ReferenceEquals(null, other))
            {
                return false;
            }

            if(ReferenceEquals(this, other))
            {
                return true;
            }

            return Latitude.Equals(other.Latitude) && Longitude.Equals(other.Longitude);
        }

        public override bool Equals(object obj)
        {
            if(ReferenceEquals(null, obj))
            {
                return false;
            }

            if(ReferenceEquals(this, obj))
            {
                return true;
            }

            if(obj.GetType() != this.GetType())
            {
                return false;
            }

            return Equals((WellLocation)obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Latitude, Longitude);
        }

        public static bool operator ==(WellLocation left,
                                       WellLocation right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(WellLocation left,
                                       WellLocation right)
        {
            return !Equals(left, right);
        }

        public int CompareTo(WellLocation other)
        {
            if(ReferenceEquals(this, other))
            {
                return 0;
            }

            if(ReferenceEquals(null, other))
            {
                return 1;
            }

            int latitudeComparison = Latitude.CompareTo(other.Latitude);

            if(latitudeComparison != 0)
            {
                return latitudeComparison;
            }

            return Longitude.CompareTo(other.Longitude);
        }

        public int CompareTo(object obj)
        {
            if(ReferenceEquals(null, obj))
            {
                return 1;
            }

            if(ReferenceEquals(this, obj))
            {
                return 0;
            }

            return obj is WellLocation other ? CompareTo(other) : throw new ArgumentException($"Object must be of type {nameof(WellLocation)}");
        }

        public static bool operator <(WellLocation left,
                                      WellLocation right)
        {
            return Comparer<WellLocation>.Default.Compare(left, right) < 0;
        }

        public static bool operator >(WellLocation left,
                                      WellLocation right)
        {
            return Comparer<WellLocation>.Default.Compare(left, right) > 0;
        }

        public static bool operator <=(WellLocation left,
                                       WellLocation right)
        {
            return Comparer<WellLocation>.Default.Compare(left, right) <= 0;
        }

        public static bool operator >=(WellLocation left,
                                       WellLocation right)
        {
            return Comparer<WellLocation>.Default.Compare(left, right) >= 0;
        }
    }
}
