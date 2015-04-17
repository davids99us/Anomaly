# Anomaly

This package is concerned with the detection and correction of anomalies in heterogeneous real-world data. Typical problems are the quality control of historic temperature data from weather stations, and early detection of 'breakouts' in cloud data or financial series. It includes a cumulative technique showing a significant improvement in the detection limit for baseline-shifts relative to a commonly used alternative. 

#UPDATES

2. Examples and stuff added to point where it builds and runs examples correctly.

1. Now consists of only three functions: anomalySeg, anomalyChow and anomalyComp.  The package data.table is so concise eliminated a lot of code.