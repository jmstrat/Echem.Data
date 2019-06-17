# List of Canonical Data Names

## Column Names

TODO: make a list of all column names and their meanings

### Echem.Process

Columns added using the `process` function:

* SpecificCapacity.Ah.
* Charge_SpecificCapacity.Ah.
* Discharge_SpecificCapacity.Ah.
* Smoothed_Voltage.V. -- The voltage used for the dQ/dV calculation
* dQdV

## Attributes

* Cycler -- The name of the cycler as given by `cycler_normalise`
* machine_number
* channel
* date -- start date as a `Date` object
* voltage_range
* current_range
* Characteristic Mass -- in mg
* monitor_version
* program_name -- aka schedule
* comment
* program -- the actual program not just the name

### Echem.Process

Attributes added using the `process` function:

* Theoretical Capacity -- mAh
* Specific Theoretical Capacity -- mAhg-1
* Characteristic Mass -- can override if already set
* dQdV-spar
* OCV -- V
* Final Voltage -- mV
* Cycles
* Current -- uA
* Capacity -- mAhg-1
* Cycle Capacity -- data frame in mAhg-1 / %
* Crate
* Charge Step
* Discharge Step

### Echem.Database

Attributes added if imported using `process.echem(<cell id>)`:

* Film-id
* Film-type -- active material name
* Film-description
* Film-mass.active
* Film-mass.carbon
* Film-mass.binder
* Film-date
* Type -- same as Film-type
