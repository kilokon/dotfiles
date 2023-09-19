
# Convert Fahrenheit to Celcius
export def f-to-c [
     fahren: number # Degrees Fahrenheit
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # (100°F − 32) × 5/9 = 37.778°C
    let $n = if ($fahren | describe) == "float" {$fahren} else {$fahren | into decimal }
    let celcius = ((( $n - 32.) * 5 / 9. ) | math round -p $round )
    $"($fahren) °F is ($celcius) °C"
}

# Convert Fahrenheit to Kelvin
export def f-to-k [
     fahren: number # Degrees Fahrenheit
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # (100°F − 32) × 5/9 + 273.15 = 310.928K

    let $n = if ($fahren | describe) == "float" {$fahren} else {$fahren | into decimal }
    let kelvin = ((($n - 32) * 5 / 9 + 273.15)| math round -p $round )
    $"($fahren) °F is ($kelvin) °K"
}

# Convert Celcius to Fahrenheit
export def c-to-f [
     celcius: number # Degrees Celcius
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # (100°C × 9/5) + 32 = 212°F

    let $n = if ($celcius | describe) == "float" {$celcius} else {$celcius | into decimal }
    let fahren = ((($n * 9 / 5) + 32) | math round -p $round )
    $"($celcius) °C is ($fahren) °F"
}

# Convert Celcius to Kelvin
export def c-to-k [
     celcius: number # Degrees Celcius
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100°C + 273.15 = 373.15K


    let $n = if ($celcius | describe) == "float" {$celcius} else {$celcius | into decimal }
    let kelvin = (($n + 273.15) | math round -p $round )
    $"($celcius) °C is ($kelvin) °K"
}

# Convert Kelvin to Fahrenheit
export def k-to-f [
     kelvin:number # Degrees Fahrenheit
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # (100K − 273.15) × 9/5 + 32 = -279.7°F

    let $n = if ($kelvin | describe) == "float" {$kelvin} else {$kelvin | into decimal }
    let fahren = ((($n - 273.15) * 9 / 5 + 32) | math round -p $round )
    $"($kelvin) °K is ($fahren) °F"
}

# Convert Kelvin to Celcius
export def k-to-c [
     kelvin:number # Degrees Celcius
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100K − 273.15 = -173.1°C

    let $n = if ($kelvin | describe) == "float" {$kelvin} else {$kelvin | into decimal }
    let celcius = (($n - 273.15) | math round -p $round )
    $"($kelvin) °K is ($celcius) °C"
}

# Convert Foot to Meter
export def ft-to-m [
     foot:number # Feet
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100ft × 0.3048 = 30.48m

    let $n = if ($foot | describe) == "float" {$foot} else {$foot | into decimal }
    let meter = (($n * 0.3048) | math round -p $round )
    $"($meter) m"
}

# Convert Meter to Foot
export def m-to-ft [
     meter:number # Meters
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100m ÷ 0.3048 = 328.084ft

    let $n = if ($meter | describe) == "float" {$meter} else {$meter | into decimal }
    let foot = (($n / 0.3048) | math round -p $round )
    $"($foot) ft"
}

# Convert Inch to Centimeter
export def in-to-cm [
     inch:number # Inches
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100in × 2.54 = 254cm

    let $n = if ($inch | describe) == "float" {$inch} else {$inch | into decimal }
    let centimeter = (($n * 2.54) | math round -p $round )
    $"($centimeter) cm"
}

# Convert Centimeter to Inch
export def cm-to-in [
     centimeter:number # Centimeters
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100cm ÷ 2.54 = 39.3701in

    let $n = if ($centimeter | describe) == "float" {$centimeter} else {$centimeter | into decimal }
    let inch = (($n / 2.54) | math round -p $round )
    $"($inch) in"
}

# Convert Mile to Kilometer
export def mi-to-km [
     mile:number # Miles
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100mi × 1.60934 = 160.934km

    let $n = if ($mile | describe) == "float" {$mile} else {$mile | into decimal }
    let kilometer = (($n * 1.60934) | math round -p $round )
    $"($kilometer) km"
}

# Convert Kilometer to Mile
export def km-to-mi [
     kilometer:number # Kilometers
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100km ÷ 1.60934 = 62.1371mi

    let $n = if ($kilometer | describe) == "float" {$kilometer} else {$kilometer | into decimal }
    let mile = (($n / 1.60934) | math round -p $round )
    $"($mile) mi"
}

# Convert Pound to Kilogram
export def lb-to-kg [
     pound:number # Pounds
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100lb × 0.453592 = 45.3592kg

    let $n = if ($pound | describe) == "float" {$pound} else {$pound | into decimal }
    let kilogram = (($n * 0.453592) | math round -p $round )
    $"($kilogram) kg"
}

# Convert Kilogram to Pound
export def kg-to-lb [
     kilogram:number # Kilograms
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100kg ÷ 0.453592 = 220.462lb

    let $n = if ($kilogram | describe) == "float" {$kilogram} else {$kilogram | into decimal }
    let pound = (($n / 0.453592) | math round -p $round )
    $"($pound) lb"
}

# Convert Ounce to Gram
export def oz-to-g [
     ounce:number # Ounces
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100oz × 28.3495 = 2834.95g

    let $n = if ($ounce | describe) == "float" {$ounce} else {$ounce | into decimal }
    let gram = (($n * 28.3495) | math round -p $round )
    $"($gram) g"
}

# Convert yard to meter
export def yd-to-m [
     yard:number # Yards
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100yd × 0.9144 = 91.44m

    let $n = if ($yard | describe) == "float" {$yard} else {$yard | into decimal }
    let meter = (($n * 0.9144) | math round -p $round )
    $"($meter) m"
}

# Convert knot to kilometer per hour
export def kn-to-kmph [
     knot:number # Knots
     --round(-r): int = 2 # Digits of precision to round to
    ] {
    # 100kn × 1.852 = 185.2km/h

    let $n = if ($knot | describe) == "float" {$knot} else {$knot | into decimal }
    let kilometer = (($n * 1.852) | math round -p $round )
    $"($kilometer) km/h"
}
