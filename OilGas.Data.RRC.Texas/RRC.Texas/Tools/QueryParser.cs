﻿using System.Collections.Generic;
using System.Linq;

using AngleSharp.Dom;
using AngleSharp.Html.Dom;

namespace OilGas.Data.RRC.Texas
{
    using static HtmlTags;

    public class QueryParser
    {
        private static WellboreQueryData ParseWellboreQueryTable(IElement rowNode)
        {
            HtmlTag xpath = tr.td[1].table.tbody.tr.td[1].a.GetRootParent();

            WellboreQueryData wellboreQueryData = new WellboreQueryData()
            {
                LeaseDetailAction = rowNode.GetElementByTags(xpath).GetAttribute()
            };

            WellboreQueryColumns wellboreQueryColumns = new WellboreQueryColumns(rowNode);

            wellboreQueryData.Columns = wellboreQueryColumns;

            return wellboreQueryData;
        }

        public static List<WellboreQueryData> ParseWellboreQuery(IHtmlDocument htmlDoc)
        {
            //IElement htmlNode = htmlDoc.DocumentElement;
            IElement bodyNode = htmlDoc.Body;

            IElement ewaPageTable = bodyNode.GetElementWithClass("ewaPage");

            IElement tabBox2Table = ewaPageTable.GetElementWithClass("TabBox2");

            IElement dataGridTable = tabBox2Table.GetElementWithClass("DataGrid");

            List<WellboreQueryData> wellboreQueryData = new List<WellboreQueryData>(1);

            IElement dataRowNode;
            int      rowNumber = 3;

            List<IElement> elements = dataGridTable.GetElementsByTags(table.tbody.tr.GetRootParent());

            do
            {
                dataRowNode = elements[rowNumber - 1];

                wellboreQueryData.Add(ParseWellboreQueryTable(dataRowNode));

                ++rowNumber;
            } while(dataRowNode != null && rowNumber <= elements.Count);

            return wellboreQueryData;
        }

        public static LeaseDetailQueryData ParseLeaseDetailQuery(IHtmlDocument htmlDoc)
        {
            string wellType = htmlDoc.
                              All.First(m => m.LocalName == strong.ToString() && m.HasAttribute("id") && m.GetAttribute("id").StartsWith("leaseDetailRcrd.oilOrGasCodeHndlr.inputValue")).
                              InnerHtml.Trim();

            LeaseDetailQueryData leaseDetailQueryData = new LeaseDetailQueryData()
            {
                LeaseType = wellType == "Oil" ? LeaseType.Oil : LeaseType.Gas
            };

            return leaseDetailQueryData;
        }

        public static List<SpecificLeaseProductionQueryData> ParseSpecificLeaseProductionQuery(IHtmlDocument htmlDoc)
        {
            return null;
        }

        //public static object extract_well_type(object lease_query_result)
        //{
        //    if(!extract_well_type.__dict__.Contains("detail_link_rgx"))
        //    {
        //        extract_well_type.detail_link_rgx = re.compile(@"href=""(leaseDetailAction.do[^""]+)""",
        //                                                       re.IGNORECASE);
        //    }
        //
        //    var match = extract_well_type.detail_link_rgx.search(lease_query_result);
        //
        //    if(!match)
        //    {
        //        throw RuntimeError("No detail link found!");
        //    }
        //
        //    var detail_url = URL_BASE + match.group(1);
        //    var request    = urllib.request.urlopen(detail_url);
        //
        //    if(request.status != 200)
        //    {
        //        throw RuntimeError("HTTP request failed.");
        //    }
        //
        //    var lease_detail = request.read().decode();
        //
        //    if(!extract_well_type.__dict__.Contains("well_type_rgx"))
        //    {
        //        extract_well_type.well_type_rgx = re.compile(@"Well Type:\s+<[^>]+>\s+(\w+)",
        //                                                     re.IGNORECASE);
        //    }
        //
        //    match = extract_well_type.well_type_rgx.search(lease_detail);
        //
        //    if(!match)
        //    {
        //        throw RuntimeError("Unable to find well type!");
        //    }
        //
        //    return match.group(1);
        //}

        //public static object parse_production_csv(object csv_data,
        //                                          object well_type)
        //{
        //    var csv_stream = io.StringIO(csv_data);
        //    var csv_reader = csv.reader(csv_stream);
        //
        //    foreach(var i in range(10))
        //    {
        //        next(csv_reader);
        //    }
        //
        //    List<object> data = new List<object>();
        //
        //    if(well_type == "Oil")
        //    {
        //        foreach(var l in csv_reader)
        //        {
        //            data.append(new Dictionary<object, object>
        //            {
        //                {
        //                    "Month", l[0]
        //                },
        //                {
        //                    "Oil Production", try_parse(l[1].
        //                                                    replace(",",
        //                                                            ""),
        //                                                float,
        //                                                0.0)
        //                },
        //                {
        //                    "Oil Disposition", try_parse(l[2].
        //                                                     replace(",",
        //                                                             ""),
        //                                                 float,
        //                                                 0.0)
        //                },
        //                {
        //                    "Gas Production", try_parse(l[3].
        //                                                    replace(",",
        //                                                            ""),
        //                                                float,
        //                                                0.0)
        //                },
        //                {
        //                    "Gas Disposition", try_parse(l[4].
        //                                                     replace(",",
        //                                                             ""),
        //                                                 float,
        //                                                 0.0)
        //                },
        //                {
        //                    "Operator", l.Count > 5 ? l[5] : data ? data[-1]["Operator"] : ""
        //                },
        //                {
        //                    "Field", l.Count > 7 ? l[7] : data ? data[-1]["Field"] : ""
        //                }
        //            });
        //        }
        //    }
        //    else if(well_type == "Gas")
        //    {
        //        foreach(var l in csv_reader)
        //        {
        //            data.append(new Dictionary<object, object>
        //            {
        //                {
        //                    "Month", l[0]
        //                },
        //                {
        //                    "Gas Production", try_parse(l[1].
        //                                                    replace(",",
        //                                                            ""),
        //                                                float,
        //                                                0.0)
        //                },
        //                {
        //                    "Gas Disposition", try_parse(l[2].
        //                                                     replace(",",
        //                                                             ""),
        //                                                 float,
        //                                                 0.0)
        //                },
        //                {
        //                    "Condensate Production", try_parse(l[3].
        //                                                           replace(",",
        //                                                                   ""),
        //                                                       float,
        //                                                       0.0)
        //                },
        //                {
        //                    "Condensate Disposition", try_parse(l[4].
        //                                                            replace(",",
        //                                                                    ""),
        //                                                        float,
        //                                                        0.0)
        //                },
        //                {
        //                    "Operator", l.Count > 5 ? l[5] : data ? data[-1]["Operator"] : ""
        //                },
        //                {
        //                    "Field", l.Count > 7 ? l[7] : data ? data[-1]["Field"] : ""
        //                }
        //            });
        //        }
        //    }
        //    else
        //    {
        //        throw RuntimeError("Invalid well type!");
        //    }
        //
        //    data.Remove(-1);
        //
        //    return data;
        //}
    }
}