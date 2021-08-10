function [amountAsleep, amountAwake, sleepEfficiency, timesAwoken, epochCapacity, epochPeak, epochPeakCounter, stormPeak, largestStorm, timesEdaStorm, meanEdaStorm, lengthEdaStorm]=EDA_Function(originalACC, originalEDA) 
	
	samplingRateEDA=originalEDA(2);
    %currentNight=originalEDA(1);
    originalEDA=originalEDA([3:end], :); % EDA Matrix without first two rows
    edaSize=length(originalEDA); % Size of the EDA Matrix without first two rows
    
    % Load ACC, apply Cole's Function to it
    %originalACC=csvread(accCSV);
    %sleepAwakeMatrix=Coles_Function(accCSV);
    sleepAwakeMatrix=Coles_Function(originalACC);
    %disp(length(sleepAwakeMatrix))
    
    % Remove all EDA data where determination is awake
    newEDA=[];
    for i=1:length(sleepAwakeMatrix)
        if (sleepAwakeMatrix(i)==0)
            newEDA=[newEDA; originalEDA((i * samplingRateEDA * 60):((i + 1) * samplingRateEDA * 60))];
        end
    end
    
    % Apply 32nd order, 4Hz FIR(Frequency Impulse Response) filter
    newEDA=transpose(newEDA);
    %newEDA=transpose(originalEDA);
    bhi=fir1(32, 0.4); 
    filteredEDA=filter(bhi, 1, newEDA);
    
    % Calculate first derivative of filtered EDA
    derMatrix=[];
    filteredEDA=transpose(filteredEDA);
    for i=2:length(newEDA)
        derMatrix=[derMatrix; ((filteredEDA(i) - filteredEDA(i - 1))/.25)];
    end
    
    % Calculate EDA Peaks per second
    peakMatrix=zeros(length(derMatrix), 1); % Use for EDA Event Feature
    edaEventCounter=0;
    i=1;
    while( i <= length(derMatrix))
        if (derMatrix(i) > 0.01) 
            peakMatrix(i)=1;
            edaEventCounter=edaEventCounter + 1;
            % Based on the paper of [Quantitative analysis of wrist
            % electrodermal activity during sleep], Peaks must be separated
            % by at least one second or they will be counted as a single
            % peak
            i=i+3;
        end
        i=i+1;
    end
    
    secondMatrix=zeros(floor(length(peakMatrix) / samplingRateEDA), 1);
    k=1;
    j=samplingRateEDA;
    while (j < length(peakMatrix))
        if (ismember(1, peakMatrix((j - (samplingRateEDA-1)):j)))
            secondMatrix(k)=1;
        end
        k=k + 1;
        j=j + samplingRateEDA;
    end
    
    % Detect all 30 second EDA Epochs
    epochMatrix=zeros(floor(length(secondMatrix) / 30), 1);
    epochPeakCounter=0;
    k=1;
    j=30;
    while (j < length(secondMatrix))
        if (ismember(1, secondMatrix((j - 29):j)))
            epochMatrix(k)=sum(secondMatrix((j - 29):j));
            epochPeakCounter=epochPeakCounter + 1;
        end
        k=k + 1;
        j=j + 30;
    end
    
    % Detect all EDA Storms
    % Showing as list of EDA Storms and their corresponding sizes
    edaStormMatrix=[];
    edaEpochStorm=0;
    i=1;
    while i < length(epochMatrix)
        if (epochMatrix(i) >= 1)
            z=i;
            tempCounter=0;
            epochSotrm=0;
            while (epochMatrix(z) >= 1)
                if (z==length(epochMatrix))
                    break;
                end
                tempCounter=tempCounter + 1;
                epochSotrm=epochSotrm+epochMatrix(z);
                z=z + 1;
            end
            if (tempCounter >= 2)
                edaStormMatrix=[edaStormMatrix; tempCounter];
                edaEpochStorm=edaEpochStorm+epochSotrm;
            end
            i=z;
        end
        i=i + 1;
    end
    
    % Calculate Sleep Efficiency
    totalTimeInBed=((edaSize / 4) / 60);
    totalTimeAsleep=((length(newEDA) / 4) / 60);
    sleepEfficiency=totalTimeAsleep/totalTimeInBed;
    
    % Some Results
    
    amountAwake=0;
    amountAsleep=0;
    timesAwoken=0;
    for i=1:length(sleepAwakeMatrix)
        if(sleepAwakeMatrix(i)==1)
            amountAwake=amountAwake + 1;
            if((i>1) && (sleepAwakeMatrix(i-1)==0))
                timesAwoken=timesAwoken+1;
            end
        else
            amountAsleep=amountAsleep + 1;
        end
    end
    
    largestStorm=0;
    for i=1:length(edaStormMatrix)
        if (edaStormMatrix(i) > largestStorm)
            largestStorm=edaStormMatrix(i);
        end
    end
	
	timesEdaStorm=length(edaStormMatrix);
	meanEdaStorm=mean(edaStormMatrix);
	%stdEdaStorm=std(edaStormMatrix);
    lengthEdaStorm=sum(edaStormMatrix);
	
    epochCapacity=sum(epochMatrix>1)/length(epochMatrix);
    epochPeak=mean(epochMatrix(find(epochMatrix>0)));
    stormPeak=(edaEpochStorm/sum(epochMatrix));
end