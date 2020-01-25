using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;

using Microsoft.Data.Analysis;

using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

using VegaLite;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    public sealed class WellProductionDate
    {
        [IgnoreDataMember]
        [NotMapped]
        public int Month { get; set; }

        [IgnoreDataMember]
        [NotMapped]
        public int Year { get; set; }

        [DataMember]
        public string Date { get { return ToString(); } }

        public WellProductionDate()
        {
        }

        public WellProductionDate(string date)
        {
            string[] parts = date.Split(' ',
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

    [Serializable]
    [DataContract]
    [Table("WellProductionRecords")]
    public sealed class WellProductionRecord : IDataTable<long>
    {
        [DataMember]
        [Required]
        [Key]
        public long Id { get; set; }

        [DataMember]
        [ForeignKey("Records")]
        public WellProduction WellProduction { get; set; }

        [DataMember]
        public int Month { get; set; }

        [DataMember]
        public float MonthlyOil { get; set; }

        [DataMember]
        public float MonthlyGas { get; set; }

        public WellProductionRecord()
        {
        }

        public WellProductionRecord(WellProduction wellProduction,
                                    int            month,
                                    float          monthlyOil,
                                    float          monthlyGas)
        {
            WellProduction = wellProduction;
            Month          = month;
            MonthlyOil     = monthlyOil;
            MonthlyGas     = monthlyGas;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public object[] ToArray()
        {
            object[] array = new object[]
            {
                Id, Month, MonthlyOil, MonthlyGas
            };

            return array;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public IEnumerable<object> ToEnumerable()
        {
            yield return Id;
            yield return Month;
            yield return MonthlyOil;
            yield return MonthlyGas;
        }
    }

    [Serializable]
    [DataContract]
    [Table("WellProduction")]
    public sealed class WellProduction : IDataTable<long>
    {
        [DataMember]
        //[Required]
        //[Key]
        public long Id { get; set; }

        [DataMember]
        public string Api { get; set; }

        [DataMember]
        public string StartDate { get; set; }

        [DataMember]
        public string EndDate { get; set; }

        [DataMember]
        public string OperatorName { get; set; }

        [DataMember]
        public string OperatorNumber { get; set; }

        [DataMember]
        public string FieldName { get; set; }

        [DataMember]
        public string FieldNumber { get; set; }

        [DataMember]
        public List<WellProductionRecord> Records { get; set; }

        public WellProduction()
        {
            Records = new List<WellProductionRecord>();
        }

        public WellProduction(ApiNumber                  api,
                              WellProductionDate         startDate,
                              WellProductionDate         endDate,
                              string                     operatorName,
                              string                     operatorNumber,
                              string                     fieldName,
                              string                     fieldNumber,
                              List<WellProductionRecord> records = null)
        {
            Api            = api.ToString();
            StartDate      = startDate.Date;
            EndDate        = endDate.Date;
            OperatorName   = operatorName;
            OperatorNumber = operatorNumber;
            FieldName      = fieldName;
            FieldNumber    = fieldNumber;
            Records        = records ?? new List<WellProductionRecord>();
        }

        internal static readonly CultureInfo    Culture    = CultureInfo.CreateSpecificCulture("en-US");
        internal static readonly DateTimeStyles Styles     = DateTimeStyles.None;
        internal const           string         DateFormat = "MMM YYYY";

        internal static WellProduction ConvertFrom(IEnumerable<SpecificLeaseProductionQueryData> queryData)
        {
            List<SpecificLeaseProductionQueryData> dataRows = queryData.ToList();

            if(dataRows.Count == 0)
            {
                throw new Exception();
            }

            SpecificLeaseProductionQueryData firstRow = dataRows.FirstOrDefault();

            if(firstRow == null)
            {
                throw new Exception();
            }

            SpecificLeaseProductionQueryData lastRow = dataRows.LastOrDefault();

            if(lastRow == null)
            {
                throw new Exception();
            }

            WellProduction wellProduction = new WellProduction(firstRow.Api,
                                                               new WellProductionDate(firstRow.Date),
                                                               new WellProductionDate(lastRow.Date),
                                                               firstRow.Operator_Name,
                                                               firstRow.Operator_No,
                                                               firstRow.Field_Name,
                                                               firstRow.Field_No);

            int month = 0;

            foreach(SpecificLeaseProductionQueryData dataRow in dataRows)
            {
                if(!float.TryParse(dataRow.OIL_BBL_Production,
                                   out float monthlyOil))
                {
                    monthlyOil = 0.0f;
                }

                if(!float.TryParse(dataRow.Casinghead_MCF_Production,
                                   out float monthlyGas))
                {
                    monthlyGas = 0.0f;
                }

                wellProduction.Records.Add(new WellProductionRecord(wellProduction,
                                                                    ++month,
                                                                    monthlyOil,
                                                                    monthlyGas));
            }

            return wellProduction;
        }

        public DataFrame ToDataFrame()
        {
            PropertyInfo[] properties = typeof(WellProductionRecord).GetProperties();

            List<DataFrameColumn> columns = new List<DataFrameColumn>(properties.Length);

            PrimitiveDataFrameColumn<int> keys = new PrimitiveDataFrameColumn<int>("Id",
                                                                                   Records.Count);

            columns.Add(keys);

            foreach(PropertyInfo property in properties)
            {
                if(property.CustomAttributes.Any(attribute => attribute.AttributeType == typeof(KeyAttribute)) ||
                   property.CustomAttributes.Any(attribute => attribute.AttributeType == typeof(ForeignKeyAttribute)))
                {
                    continue;
                }

                if(property.PropertyType == typeof(bool))
                {
                    columns.Add(new PrimitiveDataFrameColumn<bool>(property.Name,
                                                                   Records.Count));
                }
                else if(property.PropertyType == typeof(int))
                {
                    columns.Add(new PrimitiveDataFrameColumn<int>(property.Name,
                                                                  Records.Count));
                }
                else if(property.PropertyType == typeof(float))
                {
                    columns.Add(new PrimitiveDataFrameColumn<float>(property.Name,
                                                                    Records.Count));
                }
                else if(property.PropertyType == typeof(string))
                {
                    columns.Add(new StringDataFrameColumn(property.Name,
                                                          Records.Count));
                }
                else if(property.PropertyType == typeof(long))
                {
                    columns.Add(new PrimitiveDataFrameColumn<long>(property.Name,
                                                                   Records.Count));
                }
                else if(property.PropertyType == typeof(decimal))
                {
                    columns.Add(new PrimitiveDataFrameColumn<decimal>(property.Name,
                                                                      Records.Count));
                }
                else if(property.PropertyType == typeof(byte))
                {
                    columns.Add(new PrimitiveDataFrameColumn<byte>(property.Name,
                                                                   Records.Count));
                }
                else if(property.PropertyType == typeof(char))
                {
                    columns.Add(new PrimitiveDataFrameColumn<char>(property.Name,
                                                                   Records.Count));
                }
                else if(property.PropertyType == typeof(double))
                {
                    columns.Add(new PrimitiveDataFrameColumn<double>(property.Name,
                                                                     Records.Count));
                }
                else if(property.PropertyType == typeof(sbyte))
                {
                    columns.Add(new PrimitiveDataFrameColumn<sbyte>(property.Name,
                                                                    Records.Count));
                }
                else if(property.PropertyType == typeof(short))
                {
                    columns.Add(new PrimitiveDataFrameColumn<short>(property.Name,
                                                                    Records.Count));
                }
                else if(property.PropertyType == typeof(uint))
                {
                    columns.Add(new PrimitiveDataFrameColumn<uint>(property.Name,
                                                                   Records.Count));
                }
                else if(property.PropertyType == typeof(ulong))
                {
                    columns.Add(new PrimitiveDataFrameColumn<ulong>(property.Name,
                                                                    Records.Count));
                }
                else
                {
                    if(!(property.PropertyType == typeof(ushort)))
                    {
                        throw new NotSupportedException("kind");
                    }

                    columns.Add(new PrimitiveDataFrameColumn<ushort>(property.Name,
                                                                     Records.Count));
                }
            }

            DataFrame dataFrame = new DataFrame(columns);

            object[] values;

            for(int i = 0; i < Records.Count; ++i)
            {
                values = Records[i].ToArray();

                for(int j = 0; j < values.Length; ++j)
                {
                    dataFrame[i,
                              j] = Convert.ChangeType(values[j],
                                                      columns[j].DataType);
                }
            }

            return dataFrame;
        }

        public Chart BuildChart()
        {
            List<InlineDatasetElement> dataset = new List<InlineDatasetElement>();

            WellProductionRecord record;

            for(int i = 0; i < Records.Count; ++i)
            {
                record = Records[i];

                dataset.Add(new Dictionary<string, object>(4)
                {
                    {
                        "API", Api.ToString()
                    },
                    {
                        "Month", record.Month
                    },
                    {
                        "MonthlyOil", record.MonthlyOil
                    },
                    {
                        "MonthlyGas", record.MonthlyGas
                    }
                });
            }

            Specification specification = new Specification
            {
                //Description = "Stock prices of 5 Tech Companies over Time.",
                Data = new DataSource()
                {
                    Values = dataset
                },
                Encoding = new Encoding()
                {
                    X = new XClass()
                    {
                        Type = StandardType.Quantitative, Field = "Month"
                    }
                },
                Layer = new List<LayerSpec>()
                {
                    new LayerSpec()
                    {
                        Mark = new BoxPlotDefClass()
                        {
                            Type = BoxPlot.Line, Stroke = "#00FF00",
                            //Point = new OverlayMarkDef()
                            //{
                            //    Filled = false, Fill = "white"
                            //}
                        },
                        Encoding = new LayerEncoding()
                        {
                            Color = new DefWithConditionMarkPropFieldDefGradientStringNull()
                            {
                                Type = StandardType.Nominal, Field = "API"
                            },
                            Y = new YClass()
                            {
                                Type  = StandardType.Quantitative,
                                Field = "MonthlyOil",
                                Axis = new Axis()
                                {
                                    Title = "MonthlyOil (BOPD)", TitleColor = "#00FF00"
                                }
                            }
                        }
                    },
                    new LayerSpec()
                    {
                        Mark = new BoxPlotDefClass()
                        {
                            Type = BoxPlot.Line, Stroke = "#FF0000",
                            //Point = new OverlayMarkDef()
                            //{
                            //    Filled = false, Fill = "white"
                            //}
                        },
                        Encoding = new LayerEncoding()
                        {
                            Color = new DefWithConditionMarkPropFieldDefGradientStringNull()
                            {
                                Type = StandardType.Nominal, Field = "API"
                            },
                            Y = new YClass()
                            {
                                Type  = StandardType.Quantitative,
                                Field = "MonthlyGas",
                                Axis = new Axis()
                                {
                                    Title = "MonthlyGas (MSCFPD)", TitleColor = "#FF0000"
                                }
                            }
                        }
                    }
                },
                Resolve = new Resolve()
                {
                    Scale = new ScaleResolveMap()
                    {
                        Y = ResolveMode.Independent
                    }
                }
            };

            Chart chart = new Chart($"Monthly Production",
                                    specification,
                                    500,
                                    500);

            return chart;
        }

        public override string ToString()
        {
            return BuildChart().ToString();
        }
    }
}