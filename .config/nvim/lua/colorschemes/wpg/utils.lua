local M = {}

local function hex2rgb(hex)
  hex = hex:gsub("#","")
  if string.len(hex) == 3 then
    if string.len(hex) == 3 then
        return {
               tonumber("0x"..hex:sub(1,1)) * 17,
               tonumber("0x"..hex:sub(2,2)) * 17,
               tonumber("0x"..hex:sub(3,3)) * 17
             }
    else
        return {
               tonumber("0x"..hex:sub(1,2)),
               tonumber("0x"..hex:sub(3,4)),
               tonumber("0x"..hex:sub(5,6))
             }
    end
  end
end

-- Function to convert RGB to HEX string
local function rgb2hex(rgb)
    local r, g, b = unpack(rgb)
    -- Convert each RGB component to a two-digit hexadecimal string
    return string.format("#%02X%02X%02X", r, g, b)
end

-- Function to convert RGB to HSV
local function rgb2hsv(rgb)
    local r, g, b = unpack(rgb)
    r, g, b = r / 255, g / 255, b / 255
    local max, min = math.max(r, g, b), math.min(r, g, b)
    local h, s, v
    v = max

    local d = max - min
    s = max == 0 and 0 or d / max

    if max == min then
        h = 0 -- achromatic
    else
        if max == r then
            h = (g - b) / d + (g < b and 6 or 0)
        elseif max == g then
            h = (b - r) / d + 2
        elseif max == b then
            h = (r - g) / d + 4
        end
        h = h / 6
    end
    return { h, s, v }
end

-- Function to convert HSV to RGB
local function hsv2rgb(hsv)
    local h, s, v = unpack(hsv)
    local r, g, b
    local i = math.floor(h * 6)
    local f = h * 6 - i
    local p = v * (1 - s)
    local q = v * (1 - f * s)
    local t = v * (1 - (1 - f) * s)

    i = i % 6

    if i == 0 then r, g, b = v, t, p
    elseif i == 1 then r, g, b = q, v, p
    elseif i == 2 then r, g, b = p, v, t
    elseif i == 3 then r, g, b = p, q, v
    elseif i == 4 then r, g, b = t, p, v
    elseif i == 5 then r, g, b = v, p, q
    end

    return { math.floor(r * 255), math.floor(g * 255), math.floor(b * 255) }
end

-- shorthand helper function to add new values to an hsv
local function add_hsv(old_hsv, change_hsv)
    -- Calculate the new hue, ensuring it wraps around the 0-1 range
    local new_h = (old_hsv[1] + change_hsv[1]) % 1.0
    -- Calculate the new saturation, ensuring it stays within the 0-1 range
    local new_s = math.max(0, math.min(1, old_hsv[2] + change_hsv[2]))
    -- Calculate the new value, ensuring it stays within the 0-1 range
    local new_v = math.max(0, math.min(1, old_hsv[3] + change_hsv[3]))
    return {new_h, new_s, new_v}
end

-- shorthand and helper Function to calculate an intermediate vibrant hue and blend colors
M.b = function(hex_string1, hex_string2)
    local rgb1 = hex2rgb(hex_string1)
    local rgb2 = hex2rgb(hex_string2)
    local h1, s1, v1 = unpack(rgb2hsv(rgb1))
    local h2, s2, v2 = unpack(rgb2hsv(rgb2))

    local intermediate_hue = (h1 + h2) / 2 -- Simplified average for demonstration
    if math.abs(h1 - h2) > 0.5 then
        intermediate_hue = (intermediate_hue + 0.5) % 1.0
    end

    -- Use the maximum saturation and value for a more vibrant result
    local s = math.max(s1, s2)
    local v = math.max(v1, v2)

    local final_hsv = hsv2rgb({intermediate_hue, s, v})
    return rgb2hex(hsv2rgb(final_hsv))
end

-- shorthand to convert from hex string to rgb to hsv, then apply an extra hsv on top of it, then convert it back to a hex string
M.c = function(hex_string, new_hsv)
  local rgb1 = hex2rgb(hex_string)
  local hsv1 = rgb2hsv(rgb1)
  local hsv2 = add_hsv(hsv1, new_hsv)
  local rgb2 = hsv2rgb(hsv2)
  return rgb2hex(rgb2)
end

return M
