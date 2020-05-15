using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Data.Common;
using System.Runtime.Serialization;

namespace OilGas.Data.FracFocus
{
    [Serializable]
    [DataContract]
    public class Registry : IDataTable<Guid>
    {
        [Key]
        public Guid Id { get; set; }

        [DataMember, Column]
        public DateTime? JobStartDate { get; set; }

        [DataMember, Column]
        public DateTime? JobEndDate { get; set; }

        [DataMember, Column]
        public ApiNumber ApiNumber { get; set; }

        [DataMember, Column]
        public double Latitude { get; set; }

        [DataMember, Column]
        public double Longitude { get; set; }

        [DataMember, Column]
        public string Projection { get; set; }

        [DataMember, Column]
        public double? Tvd { get; set; }

        [DataMember, Column]
        public double? TotalBaseWaterVolume { get; set; }

        [DataMember, Column]
        public double? TotalBaseNonWaterVolume { get; set; }

        public Registry()
        {
        }
        
        private static T CheckAndGetValue<T>(ReadOnlySpan<string> csvData,
                                             int                  index)
        {
            string value = csvData[index];

            if(string.IsNullOrEmpty(value))
            {
                return default;
            }

            if(typeof(T) == typeof(string))
            {
                return (T)(object)value;
            }

            if(typeof(T) == typeof(bool) || typeof(T) == typeof(bool?))
            {
                return (T)(object)(value != "False");
            }

            if(typeof(T) == typeof(short) || typeof(T) == typeof(short?))
            {
                return (T)(object)short.Parse(value);
            }

            if(typeof(T) == typeof(int) || typeof(T) == typeof(int?))
            {
                return (T)(object)int.Parse(value);
            }

            if(typeof(T) == typeof(long) || typeof(T) == typeof(long?))
            {
                return (T)(object)long.Parse(value);
            }

            if(typeof(T) == typeof(float) || typeof(T) == typeof(float?))
            {
                return (T)(object)float.Parse(value);
            }

            if(typeof(T) == typeof(double) || typeof(T) == typeof(double?))
            {
                return (T)(object)double.Parse(value);
            }

            if(typeof(T) == typeof(DateTime) || typeof(T) == typeof(DateTime?))
            {
                return (T)(object)DateTime.Parse(value);
            }

            if(typeof(T) == typeof(Guid) || typeof(T) == typeof(Guid?))
            {
                return (T)(object)Guid.Parse(value);
            }

            throw new Exception();
        }

        public Registry(ReadOnlySpan<string> csvData)
        {
            int index = 0;

            Id = new Guid(csvData[index++]);

            JobStartDate = CheckAndGetValue<DateTime?>(csvData,
                                                       index++);

            JobEndDate = CheckAndGetValue<DateTime?>(csvData,
                                                     index++);

            ApiNumber = new ApiNumber(csvData[index++]).ToString();

            /*StateNumber =*/ index++;

            /*CountyNumber =*/ index++;

            /*OperatorName =*/ index++;
            /*WellName     =*/ index++;

            Latitude     = double.Parse(csvData[index++]);
            Longitude    = double.Parse(csvData[index++]);
            Projection   = csvData[index++];

            Tvd = CheckAndGetValue<double?>(csvData,
                                            index++);

            TotalBaseWaterVolume = CheckAndGetValue<double?>(csvData,
                                                             index++);

            TotalBaseNonWaterVolume = CheckAndGetValue<double?>(csvData,
                                                                index++);

            /* StateName =*/ index++;

            /*CountyName =*/ index++;

            /*FfVersion =*/ index++;

            /*FederalWell =*/ index++;

            /*IndianWell =*/ index++;

            /*Source =*/ index++;

            /*DtMod =*/ 
        }
        
        public override string ToString()
        {
            return
                $"{Id} {JobStartDate} {JobEndDate} {ApiNumber} {Latitude} {Longitude} {Projection} {Tvd} {TotalBaseWaterVolume} {TotalBaseNonWaterVolume}";
        }
    }
}