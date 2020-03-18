using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

using VegaLite;

using DateTime = System.DateTime;

namespace OilGas.Data.Charting
{
    [DebuggerDisplay("{Label}, {Day}, {Rate}, {Cumulative}")]
    public readonly struct ProductionChartData : IEquatable<ProductionChartData>, IComparable<ProductionChartData>, IComparable
    {
        [JsonProperty("Label",
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string Label { get; }

        [JsonProperty("Day",
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double Day { get; }

        [JsonProperty("Rate",
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double Rate { get; }

        [JsonProperty("Cumulative",
                      NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double Cumulative { get; }

        public ProductionChartData(string label,
                                   double day,
                                   double rate)
        {
            Label      = label;
            Day        = day;
            Rate       = rate;
            Cumulative = 0.0;
        }

        public ProductionChartData(string label,
                                   double day,
                                   double rate,
                                   double cumulative)
        {
            Label      = label;
            Day        = day;
            Rate       = rate;
            Cumulative = cumulative;
        }

        public bool Equals(ProductionChartData other)
        {
            return Label == other.Label && Day.Equals(other.Day) && Rate.Equals(other.Rate) && Cumulative.Equals(other.Cumulative);
        }

        public override bool Equals(object obj)
        {
            return obj is ProductionChartData other && Equals(other);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Label,
                                    Day,
                                    Rate,
                                    Cumulative);
        }

        public static bool operator ==(ProductionChartData left,
                                       ProductionChartData right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(ProductionChartData left,
                                       ProductionChartData right)
        {
            return !left.Equals(right);
        }

        public int CompareTo(ProductionChartData other)
        {
            int labelComparison = string.Compare(Label,
                                                 other.Label,
                                                 StringComparison.Ordinal);

            if(labelComparison != 0)
            {
                return labelComparison;
            }

            return Day.CompareTo(other.Day);
        }

        public int CompareTo(object obj)
        {
            if(ReferenceEquals(null,
                               obj))
            {
                return 1;
            }

            return obj is ProductionChartData other ? CompareTo(other) : throw new ArgumentException($"Object must be of type {nameof(ProductionChartData)}");
        }

        public static bool operator <(ProductionChartData left,
                                      ProductionChartData right)
        {
            return left.CompareTo(right) < 0;
        }

        public static bool operator >(ProductionChartData left,
                                      ProductionChartData right)
        {
            return left.CompareTo(right) > 0;
        }

        public static bool operator <=(ProductionChartData left,
                                       ProductionChartData right)
        {
            return left.CompareTo(right) <= 0;
        }

        public static bool operator >=(ProductionChartData left,
                                       ProductionChartData right)
        {
            return left.CompareTo(right) >= 0;
        }

        public override string ToString()
        {
            return $"{{Label:{Label} Day:{Day} Rate:{Rate} Cumulative:{Cumulative}}}";
        }
    }

    public sealed class ProductionChartCollection
    {
        private readonly Dictionary<string, List<ProductionChartData>> _dataSources;

//#if NETCOREAPP
//        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
//#else
//        [MethodImpl(MethodImplOptions.AggressiveInlining)]
//#endif
//        public ProductionChartCollection(IEnumerable<ProductionChartData> collection) : base(collection)
//        {
//        }

        public List<ProductionChartData> this[string label]
        {
#if NETCOREAPP
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
            get { return _dataSources[label]; }
        }

        public ProductionChartData this[string label,
                                        int    index]
        {
#if NETCOREAPP
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
            get { return _dataSources[label][index]; }
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public ProductionChartCollection()
        {
            _dataSources = new Dictionary<string, List<ProductionChartData>>();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public ProductionChartCollection(int capacity)
        {
            _dataSources = new Dictionary<string, List<ProductionChartData>>(capacity);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void Add(ProductionChartData productionChartData)
        {
            if(!_dataSources.ContainsKey(productionChartData.Label))
            {
                _dataSources.Add(productionChartData.Label,
                                 new List<ProductionChartData>());
            }

            List<ProductionChartData> values = _dataSources[productionChartData.Label];

            int index = values.FindIndex(p => Math.Abs(p.Day - productionChartData.Day) < double.Epsilon);

            if(index >= 0)
            {
                values.Insert(index,
                              productionChartData);
            }
            else
            {
                values.Add(productionChartData);
            }

            _dataSources[productionChartData.Label] = values;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void Add(string label,
                        double day,
                        double rate)
        {
            if(!_dataSources.ContainsKey(label))
            {
                _dataSources.Add(label,
                                 new List<ProductionChartData>());
            }

            List<ProductionChartData> values = _dataSources[label];

            int index = values.FindIndex(p => Math.Abs(p.Day - day) < double.Epsilon);

            ProductionChartData data = new ProductionChartData(label,
                                                               day,
                                                               rate);

            if(index >= 0)
            {
                values.Insert(index,
                              data);
            }
            else
            {
                values.Add(data);
            }

            _dataSources[label] = values;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void Add(string label,
                        double day,
                        double rate,
                        double cumulative)
        {
            if(!_dataSources.ContainsKey(label))
            {
                _dataSources.Add(label,
                                 new List<ProductionChartData>());
            }

            List<ProductionChartData> values = _dataSources[label];
            int                       index  = values.FindIndex(p => Math.Abs(p.Day - day) < double.Epsilon);

            ProductionChartData data = new ProductionChartData(label,
                                                               day,
                                                               rate,
                                                               cumulative);

            if(index >= 0)
            {
                values.Insert(index,
                              data);
            }
            else
            {
                values.Add(data);
            }

            _dataSources[label] = values;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static implicit operator ProductionChartData[](ProductionChartCollection productionChartCollection)
        {
            List<ProductionChartData> productionData = new List<ProductionChartData>(productionChartCollection._dataSources.Keys.Count * productionChartCollection._dataSources.Values.Count);

            foreach(KeyValuePair<string, List<ProductionChartData>> keyValue in productionChartCollection._dataSources)
            {
                productionData.AddRange(keyValue.Value);
            }

            return productionData.ToArray();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void BuildCumulativeProduction(int startIndex = 0)
        {
            double[] temp;

            Dictionary<string, List<ProductionChartData>> sortedDataSources = new Dictionary<string, List<ProductionChartData>>(_dataSources);

            _dataSources.Clear();

            foreach(KeyValuePair<string, List<ProductionChartData>> keyValue in sortedDataSources)
            {
                temp = keyValue.Value.Select(p => p.Rate).ToArray();

                temp = Utilities.CumulativeSum(temp,
                                               startIndex);

                for(int i = 0; i < temp.Length; i++)
                {
                    Add(new ProductionChartData(keyValue.Value[i].Label,
                                                keyValue.Value[i].Day,
                                                keyValue.Value[i].Rate,
                                                temp[i]));
                }
            }

            sortedDataSources = new Dictionary<string, List<ProductionChartData>>(_dataSources);

            foreach(KeyValuePair<string, List<ProductionChartData>> keyValue in sortedDataSources)
            {
                List<ProductionChartData> values = keyValue.Value;

                values.Sort((lhs,
                             rhs) => lhs.CompareTo(rhs));

                _dataSources[keyValue.Key] = values;
            }
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void ToMonthlyProduction(DateTime startDate,
                                        int      dayOffset)
        {
            int daysInFirstMonth = TimeSeries.DaysInMonth(startDate.Year,
                                                          startDate.Month) -
                                   dayOffset;

            Dictionary<string, List<ProductionChartData>> sortedDataSources = new Dictionary<string, List<ProductionChartData>>(_dataSources);

            _dataSources.Clear();

            double[] temp;

            foreach(KeyValuePair<string, List<ProductionChartData>> keyValue in sortedDataSources)
            {
                temp = keyValue.Value.Select(p => p.Rate).ToArray();

                Add(new ProductionChartData(keyValue.Value[0].Label,
                                            keyValue.Value[0].Day,
                                            temp[0] * daysInFirstMonth,
                                            keyValue.Value[0].Cumulative));

                for(int i = 1; i < temp.Length; i++)
                {
                    Add(new ProductionChartData(keyValue.Value[i].Label,
                                                keyValue.Value[i].Day,
                                                temp[i] * (keyValue.Value[i].Day - keyValue.Value[i - 1].Day),
                                                keyValue.Value[i].Cumulative));
                }
            }

            sortedDataSources = new Dictionary<string, List<ProductionChartData>>(_dataSources);

            foreach(KeyValuePair<string, List<ProductionChartData>> keyValue in sortedDataSources)
            {
                List<ProductionChartData> values = keyValue.Value;

                values.Sort((lhs,
                             rhs) => lhs.CompareTo(rhs));

                _dataSources[keyValue.Key] = values;
            }
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static Specification DefaultSpecification(string datasetName)
        {
            Specification specification = new Specification
            {
                Data = new DataSource
                {
                    Name = datasetName
                },
                Encoding = new Encoding
                {
                    X = new XClass
                    {
                        Type = StandardType.Quantitative,
                        Field = "Day",
                        Axis = new Axis()
                        {
                            Title     = "Time (Day)",
                            MinExtent = 0.0
                        }
                    }
                },
                Layer = new List<LayerSpec>
                {
                    new LayerSpec
                    {
                        Mark = new BoxPlotDefClass
                        {
                            Type = BoxPlot.Line, Point = true
                        },
                        Encoding = new LayerEncoding
                        {
                            Y = new YClass
                            {
                                Type = StandardType.Quantitative, Field = "Rate",
                                Axis = new Axis()
                                {
                                    Title = "Rate (BOE)",
                                    MinExtent = 0.0
                                }
                                
                            },
                            Color = new DefWithConditionMarkPropFieldDefGradientStringNull
                            {
                                Type = StandardType.Nominal, Field = "Label"
                            }
                        }
                    },
                    new LayerSpec
                    {
                        Mark = new BoxPlotDefClass
                        {
                            Type = BoxPlot.Line, Point = true
                        },
                        Encoding = new LayerEncoding
                        {
                            Y = new YClass
                            {
                                Type  = StandardType.Quantitative,
                                Field = "Cumulative",
                                Axis = new Axis
                                {
                                    Title = "Cumulative  (BOE)",
                                    MinExtent = 0.0
                                }
                            },
                            Color = new DefWithConditionMarkPropFieldDefGradientStringNull
                            {
                                Type = StandardType.Nominal, Field = "Label"
                            },
                            Tooltip = new List<StringFieldDef>
                            {
                                new StringFieldDef
                                {
                                    Type = StandardType.Quantitative, Field = "Cumulative"
                                }
                            }
                        }
                    }
                },
                Resolve = new Resolve
                {
                    Scale = new ScaleResolveMap
                    {
                        Y = ResolveMode.Independent
                    }
                }
            };

            return specification;
        }
    }
}
