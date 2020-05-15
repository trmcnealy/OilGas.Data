using System;
using System.Collections.Generic;
using System.Runtime.Serialization;


namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    public class DirectionalSurveyReport : IEquatable<DirectionalSurveyReport>, IComparable<DirectionalSurveyReport>, IComparable
    {
        [DataMember]
        public double From { get; set; }
        
        [DataMember]
        public double To { get; set; }

        [IgnoreDataMember]
        public double TotalDepth { get { return To - From; } }


        //PDFs
        
        public DirectionalSurveyReport(string @from,
                                 string to)
        {
            if(!string.IsNullOrEmpty(@from))
            {
                From = double.Parse(@from);
            }

            if(!string.IsNullOrEmpty(to))
            {
                To = double.Parse(to);
            }
        }

        public DirectionalSurveyReport(double @from,
                                 double to)
        {
            From = @from;
            To   = to;
        }

        public bool Equals(DirectionalSurveyReport other)
        {
            if(ReferenceEquals(null, other))
            {
                return false;
            }

            if(ReferenceEquals(this, other))
            {
                return true;
            }

            return From.Equals(other.From) && To.Equals(other.To);
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

            return Equals((DirectionalSurveyReport)obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(From, To);
        }

        public static bool operator ==(DirectionalSurveyReport left,
                                       DirectionalSurveyReport right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(DirectionalSurveyReport left,
                                       DirectionalSurveyReport right)
        {
            return !Equals(left, right);
        }

        public int CompareTo(DirectionalSurveyReport other)
        {
            if(ReferenceEquals(this, other))
            {
                return 0;
            }

            if(ReferenceEquals(null, other))
            {
                return 1;
            }

            return TotalDepth.CompareTo(other.TotalDepth);
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

            return obj is DirectionalSurveyReport other ? CompareTo(other) : throw new ArgumentException($"Object must be of type {nameof(DirectionalSurveyReport)}");
        }

        public static bool operator <(DirectionalSurveyReport left,
                                      DirectionalSurveyReport right)
        {
            return Comparer<DirectionalSurveyReport>.Default.Compare(left, right) < 0;
        }

        public static bool operator >(DirectionalSurveyReport left,
                                      DirectionalSurveyReport right)
        {
            return Comparer<DirectionalSurveyReport>.Default.Compare(left, right) > 0;
        }

        public static bool operator <=(DirectionalSurveyReport left,
                                       DirectionalSurveyReport right)
        {
            return Comparer<DirectionalSurveyReport>.Default.Compare(left, right) <= 0;
        }

        public static bool operator >=(DirectionalSurveyReport left,
                                       DirectionalSurveyReport right)
        {
            return Comparer<DirectionalSurveyReport>.Default.Compare(left, right) >= 0;
        }
    }
}
