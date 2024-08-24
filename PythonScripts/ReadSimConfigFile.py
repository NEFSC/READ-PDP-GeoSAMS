#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
def ReadSimConfigFile(simCfgFile):
    # need to read Configuration/Simulation/Scallop.cfg to determine which parameters were output
    tsInYear = 0
    paramStr = []
    with open(simCfgFile, 'r') as f:
        while True:
            line = f.readline()
            if not line:
                f.close()
                break
            if (line[0] != '#'):
                j = line.find('=')
                tag = line[0:j].strip()
                k = line.find('#')
                if (k == 0):
                    k = len(line)
                value = line[j+1:k].strip()

                # Python 3.8 does not have match/case so using if elif
                if (tag == 'Select Abundance'):
                    paramStr.append('ABUN_')
                elif (tag == 'Select BMS'):
                    paramStr.append('BIOM_')
                elif (tag == 'Select Expl BMS'):
                    paramStr.append('EBMS_')
                elif (tag == 'Select Fishing Effort'):
                    paramStr.append('FEFF_')
                elif (tag == 'Select Fishing Mortality'):
                    paramStr.append('FMOR_')
                elif (tag == 'Select Landings by Number'):
                    paramStr.append('LAND_')
                elif (tag == 'Select Landings by Weight'):
                    paramStr.append('LNDW_')
                elif (tag == 'Select LPUE'):
                    paramStr.append('LPUE_')
                elif (tag == 'Select RECR'):
                    paramStr.append('RECR_')
                elif (tag == 'Time steps per Year'):
                    tsInYear = int(value)

    return [paramStr, tsInYear]


