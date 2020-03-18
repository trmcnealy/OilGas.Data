using System;

namespace OilGas.Data.Charting
{
    public static class TimeSeries
    {
        private static readonly int[][] daysInMonth =
        {
            new int[]
            {
                31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            },
            new int[]
            {
                31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            },
            new int[]
            {
                31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            },
            new int[]
            {
                31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            }
        };

        private static readonly int[][] daysInYear =
        {
            new int[]
            {
                31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366
            },
            new int[]
            {
                31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
            },
            new int[]
            {
                31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
            },
            new int[]
            {
                31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
            }
        };

        public static int DaysInMonth(this DateTime dateTime)
        {
            return daysInMonth[dateTime.Year % 4][dateTime.Month];
        }

        public static int DaysInMonth(int year,
                                      int month)
        {
            return daysInMonth[year % 4][(month-1) % 12];
        }

        //public static DateTime AddMonths(this DateTime dateTime,
        //                                 int           months)
        //{
        //    int day   = dateTime.Day;
        //    int month = dateTime.Month;
        //    int year  = dateTime.Year;

        //    int dayOffset = dateTime.DayOfYear;

        //    int daysInCurrentMonth = DaysInMonth(year,
        //                                         month);

        //    int daysUntilEndOfMonth = daysInCurrentMonth - day;

        //    for(int i = 0; i < months; i++)
        //    {
        //        dayOffset += DaysInMonth(year,
        //                                 month + i);
        //    }

        //    return dateTime.AddMonths();
        //}
    }
}
