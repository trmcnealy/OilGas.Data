using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Microsoft.Data.Analysis;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

using VegaLite;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    [XmlRoot("WellProduction")]
    public sealed class WellProduction : IDataTable<int>, IEquatable<WellProduction>
    {
        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        //[Key]
        public int Id { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty("API",
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string Api { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(StartDate),
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string StartDate { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(EndDate),
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string EndDate { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OperatorName),
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string OperatorName { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OperatorNumber),
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string OperatorNumber { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FieldName),
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string FieldName { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FieldNumber),
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string FieldNumber { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(Records),
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
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

        public WellProductionRecord[] GetProductionForVegaChart()
        {
            WellProductionRecord[] records = Records.ToArray();

            for(int i = 0; i < records.Length; ++i)
            {
                records[i].WellProduction = this;
            }

            return records;
        }

        public static Specification DefaultSpecification(string datasetName)
        {
            Specification specification = new Specification
            {
                //Description = "Stock prices of 5 Tech Companies over Time.",
                Data = new DataSource()
                {
                    Name = datasetName
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
                            Type = BoxPlot.Line, Stroke = "#00FF00"
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
                            Type = BoxPlot.Line, Stroke = "#FF0000"
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

            return specification;
        }

        public bool Equals(WellProduction other)
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

            return Id             == other.Id             &&
                   Api            == other.Api            &&
                   StartDate      == other.StartDate      &&
                   EndDate        == other.EndDate        &&
                   OperatorName   == other.OperatorName   &&
                   OperatorNumber == other.OperatorNumber &&
                   FieldName      == other.FieldName      &&
                   FieldNumber    == other.FieldNumber    &&
                   Equals(Records,
                          other.Records);
        }

        public override bool Equals(object obj)
        {
            return ReferenceEquals(this,
                                   obj) ||
                   obj is WellProduction other && Equals(other);
        }

        public override int GetHashCode()
        {
            HashCode hashCode = new HashCode();
            hashCode.Add(Api);
            hashCode.Add(StartDate);
            hashCode.Add(EndDate);
            hashCode.Add(OperatorName);
            hashCode.Add(OperatorNumber);
            hashCode.Add(FieldName);
            hashCode.Add(FieldNumber);
            hashCode.Add(Records);

            return hashCode.ToHashCode();
        }

        public static bool operator ==(WellProduction left,
                                       WellProduction right)
        {
            return Equals(left,
                          right);
        }

        public static bool operator !=(WellProduction left,
                                       WellProduction right)
        {
            return !Equals(left,
                           right);
        }
    }
}
