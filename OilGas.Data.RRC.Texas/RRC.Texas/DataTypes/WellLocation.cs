using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations.Schema;

using OilGas.Data.CoordinateSystems;

namespace OilGas.Data.RRC.Texas
{
    [NotMapped]
    public class WellLocation : IEquatable<WellLocation>, IComparable<WellLocation>, IComparable
    {
        public LatitudeLongitude LatitudeLongitude { get; }

        public string Projection { get; }

        public WellLocation(double latitude,
                            double longitude,
                            string projection)
        {
            LatitudeLongitude = new LatitudeLongitude(latitude,longitude);
            Projection = projection;
        }

        public override string ToString()
        {
            return $"{LatitudeLongitude}[{Projection}]";
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

            return LatitudeLongitude.Equals(other.LatitudeLongitude) && Projection == other.Projection;
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
            return HashCode.Combine(LatitudeLongitude, Projection);
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

            return LatitudeLongitude.CompareTo(other.LatitudeLongitude);
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
