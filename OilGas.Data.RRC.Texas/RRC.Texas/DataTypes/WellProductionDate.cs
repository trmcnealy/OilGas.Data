﻿using System;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Engineering.DataSource;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    [XmlType]
    public sealed class WellProductionDate
    {
        [IgnoreDataMember, XmlIgnore]
        public int Month { get; set; }

        [IgnoreDataMember, XmlIgnore]
        public int Year { get; set; }

        [DataMember]
        [XmlElement]
        public string Date { get { return ToString(); } }

        public WellProductionDate()
        {
        }

        public WellProductionDate(string date)
        {
            string[] parts = date.Split(new char[]
                                        {
                                            ' '
                                        },
                                        StringSplitOptions.RemoveEmptyEntries);

            if(!int.TryParse(parts[0],
                             out int month))
            {
                month = (int)MonthType.FromMonthName(parts[0]);
            }

            Month = month;

            if(!int.TryParse(parts[1],
                             out int year))
            {
                year = 1993;
            }

            Year = year;
        }

        public WellProductionDate(string month,
                                  string year)
        {
            if(!string.IsNullOrEmpty(month))
            {
                Month = (int)MonthType.FromMonthName(month);
            }
            else
            {
                Month = 1;
            }

            if(!string.IsNullOrEmpty(year))
            {
                if(!int.TryParse(year,
                                 out int _year))
                {
                    _year = 1993;
                }

                Year = _year;
            }
            else
            {
                Year = 1993;
            }
        }

        public override string ToString()
        {
            return $"{Month} {Year}";
        }
    }
}