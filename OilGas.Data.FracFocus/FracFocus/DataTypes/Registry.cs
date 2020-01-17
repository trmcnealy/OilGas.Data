using System;
using System.Globalization;

using LiteDB;

namespace OilGas.Data.FracFocus
{
    public class Registry : IDataTable
    {
        [BsonId(true)]
        public long Id { get; set; }

        /// <summary>
        /// Key index for the table
        /// </summary>
        public Guid Key { get; set; }

        /// <summary>
        /// The date on which the hydraulic fracturing job was initiated.  Does not include site preparation or setup.
        /// </summary>
        public DateTime JobStartDate { get; set; }

        /// <summary>
        /// The date on which the hydraulic fracturing job was completed.  Does not include site teardown.
        /// </summary>
        public DateTime JobEndDate { get; set; }

        /// <summary>
        /// The American Petroleum Institute well identification number formatted as follows xx-xxx-xxxxx0000 Where: First two digits
        /// represent the state, second three digits represent the county, third 5 digits represent the well.
        /// </summary>
        public ApiNumber ApiNumber { get; set; }

        /// <summary>
        /// The first two digits of the API number.  Range is from 01-50.
        /// </summary>
        [BsonIgnore]
        public string StateNumber { get; set; }

        /// <summary>
        /// The 3 digit county code.
        /// </summary>
        [BsonIgnore]
        public string CountyNumber { get; set; }

        /// <summary>
        /// The name of the operator.
        /// </summary>
        public string OperatorName { get; set; }

        /// <summary>
        /// The name of the well.
        /// </summary>
        public string WellName { get; set; }

        /// <summary>
        /// The lines that circle the earth horizontally, running side to side at equal distances apart on the earth.   Latitude is typically
        /// expressed in degrees North/ South.  In the FracFocus system these lines are shown in decimal degrees and must be between 15 and 75.
        /// </summary>
        public string Latitude { get; set; }

        /// <summary>
        /// The lines that circle the earth vertically, running top to bottom that are equal distances apart at the equator
        /// and merge at    the geographic      top and      bottom of            the earth.  Longitude is typically expressed in degrees East/ West.  In                       the FracFocus
        /// system the  number representing these  lines are shown in decimal degrees and                        must be              between -180 and -163 Note: Longitude number must
        /// be preceded by a                negative sign.
        /// </summary>
        public string Longitude { get; set; }

        /// <summary>
        /// The geographic coordinate system to which the latitude and longitude are related. In the FracFocus
        /// system the projection systems allowed are NAD (North American Datum) 27 or 83 and UTM (Universal Transverse Mercator).
        /// </summary>
        public string Projection { get; set; }

        /// <summary>
        /// The vertical distance from a point in the well (usually the current or final depth) to a point at the surface, usually the
        /// elevation of the rotary kelly bushing.
        /// </summary>
        public string Tvd { get; set; }

        /// <summary>
        /// The total volume of water used as a carrier fluid for the hydraulic fracturing job (in gallons).
        /// </summary>
        public string TotalBaseWaterVolume { get; set; }

        /// <summary>
        /// The total volume of non water components used as a carrier fluid for the hydraulic fracturing job (in gallons)
        /// </summary>
        public string TotalBaseNonWaterVolume { get; set; }

        /// <summary>
        /// The name of the state where the surface location of the well resides.  Calculated from the API number.
        /// </summary>
        public string StateName { get; set; }

        /// <summary>
        /// The name of the county were the surface location of the well resides.  Calculated from the API number.
        /// </summary>
        public string CountyName { get; set; }

        /// <summary>
        /// A key which designates which version of FracFocus was used when the disclosure was submitted.
        /// </summary>
        public string FfVersion { get; set; }

        public bool FederalWell { get; set; }

        public bool IndianWell { get; set; }

        public string Source { get; set; }

        public string DTMOD { get; set; }

        public Registry()
        {
        }

        public Registry(ReadOnlySpan<string> csvData)
        {
            if(csvData.Length != 21)
            {
                throw new Exception("Csv row must have 21 columns.");
            }

            int index = 0;
            Key = new Guid(csvData[index++]);

            string startDate = csvData[index++];
            string endDate   = csvData[index++];

            if(!string.IsNullOrEmpty(startDate))
            {
                JobStartDate = DateTime.Parse(startDate);
                JobEndDate   = DateTime.Parse(endDate);
            }
            else
            {
                JobEndDate   = DateTime.Parse(endDate);
                JobStartDate = JobEndDate;
            }

            ApiNumber               = csvData[index++];
            StateNumber             = csvData[index++];
            CountyNumber            = csvData[index++];
            OperatorName            = csvData[index++];
            WellName                = csvData[index++];
            Latitude                = csvData[index++];
            Longitude               = csvData[index++];
            Projection              = csvData[index++];
            Tvd                     = csvData[index++];
            TotalBaseWaterVolume    = csvData[index++];
            TotalBaseNonWaterVolume = csvData[index++];
            StateName               = csvData[index++];
            CountyName              = csvData[index++];
            FfVersion               = csvData[index++];
            FederalWell             = csvData[index++] != "False";
            IndianWell              = csvData[index++] != "False";
            Source                  = csvData[index++];
            DTMOD                   = csvData[index];
        }
    }
}