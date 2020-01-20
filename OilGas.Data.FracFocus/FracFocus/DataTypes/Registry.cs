using System;
using System.Data.Common;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace OilGas.Data.FracFocus
{
    [Serializable]
    [DataContract]
    [Table("Registry")]
    public class Registry : IDataTable<Guid>
    {
        /// <summary>
        /// Key index for the table
        /// </summary>
        [DataMember, Required, Key]
        public Guid Id { get; set; }

        /// <summary>
        /// The date on which the hydraulic fracturing job was initiated.  Does not include site preparation or setup.
        /// </summary>
        [DataMember]
        public DateTime? JobStartDate { get; set; }

        /// <summary>
        /// The date on which the hydraulic fracturing job was completed.  Does not include site teardown.
        /// </summary>
        [DataMember]
        public DateTime? JobEndDate { get; set; }

        /// <summary>
        /// The American Petroleum Institute well identification number formatted as follows xx-xxx-xxxxx0000 Where: First two digits
        /// represent the state, second three digits represent the county, third 5 digits represent the well.
        /// </summary>
        [DataMember]
        public string ApiNumber { get; set; }

        /// <summary>
        /// The first two digits of the API number.  Range is from 01-50.
        /// </summary>
        [DataMember]
        public string StateNumber { get; set; }

        /// <summary>
        /// The 3 digit county code.
        /// </summary>
        [DataMember]
        public string CountyNumber { get; set; }

        /// <summary>
        /// The name of the operator.
        /// </summary>
        [DataMember]
        public string OperatorName { get; set; }

        /// <summary>
        /// The name of the well.
        /// </summary>
        [DataMember]
        public string WellName { get; set; }

        /// <summary>
        /// The lines that circle the earth horizontally, running side to side at equal distances apart on the earth.   Latitude is typically
        /// expressed in degrees North/ South.  In the FracFocus system these lines are shown in decimal degrees and must be between 15 and 75.
        /// </summary>
        [DataMember]
        public double Latitude { get; set; }

        /// <summary>
        /// The lines that circle the earth vertically, running top to bottom that are equal distances apart at the equator
        /// and merge at the geographic top and bottom of the earth.  Longitude is typically expressed in degrees East/ West.  In the FracFocus
        /// system the number representing these  lines are shown in decimal degrees and must be between -180 and -163 Note: Longitude number must
        /// be preceded by a negative sign.
        /// </summary>
        [DataMember]
        public double Longitude { get; set; }

        /// <summary>
        /// The geographic coordinate system to which the latitude and longitude are related. In the FracFocus
        /// system the projection systems allowed are NAD (North American Datum) 27 or 83 and UTM (Universal Transverse Mercator).
        /// </summary>
        [DataMember]
        public string Projection { get; set; }

        /// <summary>
        /// The vertical distance from a point in the well (usually the current or final depth) to a point at the surface, usually the
        /// elevation of the rotary kelly bushing.
        /// </summary>
        [DataMember]
        public double? Tvd { get; set; }

        /// <summary>
        /// The total volume of water used as a carrier fluid for the hydraulic fracturing job (in gallons).
        /// </summary>
        [DataMember]
        public double? TotalBaseWaterVolume { get; set; }

        /// <summary>
        /// The total volume of non water components used as a carrier fluid for the hydraulic fracturing job (in gallons)
        /// </summary>
        [DataMember]
        public double? TotalBaseNonWaterVolume { get; set; }

        /// <summary>
        /// The name of the state where the surface location of the well resides.  Calculated from the API number.
        /// </summary>

        [DataMember]
        public string StateName { get; set; }

        /// <summary>
        /// The name of the county were the surface location of the well resides.  Calculated from the API number.
        /// </summary>

        [DataMember]
        public string CountyName { get; set; }

        /// <summary>
        /// A key which designates which version of FracFocus was used when the disclosure was submitted.
        /// </summary>
        [DataMember]
        public float? FfVersion { get; set; }

        [DataMember]
        public bool? FederalWell { get; set; }

        [DataMember]
        public bool IndianWell { get; set; }

        [DataMember]
        public string Source { get; set; }

        [DataMember]
        public DateTime? DtMod { get; set; }

        public Registry()
        {
        }

        //public Registry(ReadOnlySpan<string> csvData)
        //{
        //    if (csvData.Length != 21)
        //    {
        //        throw new Exception("Csv row must have 21 columns.");
        //    }
        //
        //    int index = 0;
        //    Key = new Guid(csvData[index++]);
        //
        //    string startDate = csvData[index++];
        //    string endDate = csvData[index++];
        //
        //    if (!string.IsNullOrEmpty(startDate))
        //    {
        //        JobStartDate = DateTime.Parse(startDate);
        //        JobEndDate = DateTime.Parse(endDate);
        //    }
        //    else
        //    {
        //        JobEndDate = DateTime.Parse(endDate);
        //        JobStartDate = JobEndDate;
        //    }
        //
        //    ApiNumber = csvData[index++];
        //    StateNumber = csvData[index++];
        //    CountyNumber = csvData[index++];
        //    OperatorName = csvData[index++];
        //    WellName = csvData[index++];
        //    Latitude = double.Parse(csvData[index++]);
        //    Longitude = double.Parse(csvData[index++]);
        //    Projection = csvData[index++];
        //    Tvd = double.Parse(csvData[index++]);
        //    TotalBaseWaterVolume = csvData[index++];
        //    TotalBaseNonWaterVolume = csvData[index++];
        //    StateName = csvData[index++];
        //    CountyName = csvData[index++];
        //    FfVersion = csvData[index++];
        //    FederalWell = csvData[index++] != "False";
        //    IndianWell = csvData[index++] != "False";
        //    Source = csvData[index++];
        //    DtMod = csvData[index];
        //}

        private static T CheckAndGetValue<T>(DbDataReader dbReader,
                                             int          index)
        {
            object value = dbReader.GetValue(index);

            if(DBNull.Value == value)
            {
                return default;
            }

            return (T)value;
        }

        private static T CheckAndGetValue<T>(ReadOnlySpan<string> csvData,
                                             int index)
        {
            string value = csvData[index];

            if(string.IsNullOrEmpty(value))
            {
                return default;
            }

            if(typeof(T) == typeof(bool) || typeof(T) == typeof(bool?))
            {
                return (T)(object)(value != "False");
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
        
#nullable enable
        public Registry(ReadOnlySpan<string> csvData)
        {
            int index = 0;
            Id = new Guid(csvData[index++]);

            JobStartDate = CheckAndGetValue<DateTime?>(csvData,
                                                       index++);

            JobEndDate = CheckAndGetValue<DateTime?>(csvData,
                                                     index++);

            ApiNumber = csvData[index++];

            StateNumber = CheckAndGetValue<string?>(csvData,
                                                    index++);

            CountyNumber = CheckAndGetValue<string?>(csvData,
                                                     index++);

            OperatorName = csvData[index++];
            WellName     = csvData[index++];
            Latitude     = double.Parse(csvData[index++]);
            Longitude    = double.Parse(csvData[index++]);
            Projection   = csvData[index++];

            Tvd = CheckAndGetValue<double?>(csvData,
                                            index++);

            TotalBaseWaterVolume = CheckAndGetValue<double?>(csvData,
                                                             index++);

            TotalBaseNonWaterVolume = CheckAndGetValue<double?>(csvData,
                                                                index++);

            StateName = CheckAndGetValue<string?>(csvData,
                                                  index++);

            CountyName = CheckAndGetValue<string?>(csvData,
                                                   index++);

            FfVersion = CheckAndGetValue<float?>(csvData,
                                                 index++);

            FederalWell = CheckAndGetValue<bool?>(csvData,
                                                  index++);

            IndianWell = csvData[index++] != "False";

            Source = CheckAndGetValue<string?>(csvData,
                                               index++);

            DtMod = CheckAndGetValue<DateTime>(csvData,
                                               index);
        }

        public Registry(DbDataReader dbReader)
        {
            int index = 0;
            Id = dbReader.GetGuid(index++);

            JobStartDate = CheckAndGetValue<DateTime?>(dbReader,
                                                       index++);

            JobEndDate = CheckAndGetValue<DateTime?>(dbReader,
                                                     index++);

            ApiNumber = dbReader.GetString(index++);

            StateNumber = CheckAndGetValue<string?>(dbReader,
                                                    index++);

            CountyNumber = CheckAndGetValue<string?>(dbReader,
                                                     index++);

            OperatorName = dbReader.GetString(index++);
            WellName     = dbReader.GetString(index++);
            Latitude     = dbReader.GetDouble(index++);
            Longitude    = dbReader.GetDouble(index++);
            Projection   = dbReader.GetString(index++);

            Tvd = CheckAndGetValue<double?>(dbReader,
                                            index++);

            TotalBaseWaterVolume = CheckAndGetValue<double?>(dbReader,
                                                             index++);

            TotalBaseNonWaterVolume = CheckAndGetValue<double?>(dbReader,
                                                                index++);

            StateName = CheckAndGetValue<string?>(dbReader,
                                                  index++);

            CountyName = CheckAndGetValue<string?>(dbReader,
                                                   index++);

            FfVersion = CheckAndGetValue<float?>(dbReader,
                                                 index++);

            FederalWell = CheckAndGetValue<bool?>(dbReader,
                                                  index++);

            IndianWell = dbReader.GetBoolean(index++);

            Source = CheckAndGetValue<string?>(dbReader,
                                               index++);

            DtMod = CheckAndGetValue<DateTime>(dbReader,
                                               index);
        }
#nullable disable

        public override string ToString()
        {
            return $"{Id} {JobStartDate} {JobEndDate} {ApiNumber} {StateNumber} {CountyNumber} {OperatorName} {WellName} {Latitude} {Longitude} {Projection} {Tvd} {TotalBaseWaterVolume} {TotalBaseNonWaterVolume} {StateName} {CountyName} {FfVersion} {FederalWell} {IndianWell} {Source} {DtMod}";
        }
    }
}