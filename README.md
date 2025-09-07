# Code base to interrogate and monitor PREMAP recruitment

Scripts depend on RECcapDM R package which enables an API connection to REDCap ensuring the latest data is available and data structures are consistent. 
The scripting and version control of project monitoring ensures that results are always traceable and reproducible. 

## Development phase
- Early stage, this represents the initial systematic analysis of PREMAP project progress in generating its target data.
- A key objective of this work is to identify and fix issues in data collection to maximise the value of the final project dataset.
- All results are subject to updating as we develop this work.

## List of scripts and functionality
- To come...

## User guidance
- The underlying REDCap relational database is complex, with multiple tables that must be correctly navigated to understand the data correctly.
- Key REDCapDM functions are: redcap_data() which connects to the database, and rd_transform(... , final_format="by_event") which restructures it for easiest analysis in most use cases.
  The exceptional cases are when 1) data from multiple events must be analysed together; or 2) the record of instrument completions (a field in REDCap that sits outside the programmed instruments) is to be used. 

