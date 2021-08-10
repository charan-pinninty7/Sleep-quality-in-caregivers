function [outputMatrix]=Coles_Function(originalACC)
% Load the csv file and create variables
samplingRate=originalACC(2)/2;
samplingRate = originalACC(2) / 2;
originalACC = originalACC([3:end], :);
size = floor(length(originalACC) / 2);
outputMatrix = ones(floor((((size) / samplingRate) / 60)), 1);
zcd = dsp.ZeroCrossingDetector;

% Smooth from 32 to 16 Hz
downSampledACC = zeros((size), 3);
meanVar = 0;
i = 0;
j = 1;
for x = (1:length(originalACC))
    if (j == ((size) + 1))
        break;
    end
    meanVar = meanVar + originalACC(x, :);
    i = i + 1;
    if (i == 2)
        meanVar = meanVar / 2;
        downSampledACC(j, :) = meanVar;
        i = 0;
        j = j + 1;
        meanVar = 0;
    end
end

% Variables specific to coles function
startingEpoch = 1;
endingEpoch = 10;
activityMatrix = zeros(7, 1);

% Peform the calculations of the cole function
for k = 5:(length(outputMatrix) - 4)
    currentPosition = ((k * 60) * samplingRate);
    matrixPointer = 1;
    for z = -4:2
        largestEpoch = 0;
        tempCurrentPosition = currentPosition + (z * samplingRate * 60);
        for i = 1:26
            tempPosition = 0;
            if i == 1
                tempPosition = tempCurrentPosition;
            else
                tempPosition = tempCurrentPosition + (startingEpoch * samplingRate);
            end
            tempMatrix = downSampledACC(tempPosition:(tempPosition + (endingEpoch * samplingRate)), :);
            % Use zero crossing algorithm to calculate wrist activity
            tempValue = sum(step(zcd, tempMatrix));
            if tempValue > largestEpoch
                largestEpoch = tempValue;
            end
            zcd.release();
            
            startingEpoch = startingEpoch + 2;
            endingEpoch = endingEpoch + 2;
        end
        activityMatrix(matrixPointer) = largestEpoch;
        startingEpoch = 1;
        endingEpoch = 10;
        matrixPointer = matrixPointer + 1;
        largestEpoch = 0;
    end
    
    % Determine if minute is asleep or awake
    awakeAsleep = -1;
    D = 0.00001 * ((404 * activityMatrix(1, 1)) + (598 * activityMatrix(2, 1)) + (326 * activityMatrix(3, 1)) + (441 * activityMatrix(4, 1)) + (1408 * activityMatrix(5, 1)) + (508 * activityMatrix(6, 1)) + (350 * activityMatrix(7, 1)));
    if D < 1
        awakeAsleep = 0;
    else
        awakeAsleep = 1;
    end
    outputMatrix(k) = awakeAsleep;
    activityMatrix = zeros(7, 1);
end

% Implement rescoring
% Rule A
i = 4;
while i < (length(outputMatrix))
    if ((outputMatrix((i - 3):i) == 1) & (outputMatrix(i + 1) == 1))
        i = i + 1;
    elseif (outputMatrix((i - 3):i) == 1)
        outputMatrix(i + 1) = 1;
        i = i + 5;
    else
        i = i + 1;
    end
end

% Rule B
i = 10;
while i < (length(outputMatrix))
    if ((outputMatrix((i - 9):i) == 1) & (outputMatrix(i + 1) == 1))
        i = i + 1;
    elseif (outputMatrix((i - 9):i) == 1)
        outputMatrix(i + 1) = 1;
        i = i + 11;
    else
        i = i + 1;
    end
end

% Rule C
i = 15;
while i < (length(outputMatrix))
    if (outputMatrix((i - 14):i) == 1  & (outputMatrix(i + 1) == 1))
        i = i + 1;
    elseif (outputMatrix((i - 9):i) == 1)
        outputMatrix(i + 1) = 1;
        i = i + 16;
    else
        i = i + 1;
    end
end

% Rule D (Can be optimized)
i = 1;
while i < (length(outputMatrix))
    if (outputMatrix(i) == 1)
        position = i;
        tempPosition = i;
        if(tempPosition == length(outputMatrix))
            break;
        end
        oneCounter = 0;
        while (outputMatrix(tempPosition) == 1)
            if(tempPosition == length(outputMatrix))
                break;
            end
            tempPosition = tempPosition + 1;
            oneCounter = oneCounter + 1;
        end
        zeroCounter = 0;
        if (oneCounter >= 10)
            while (outputMatrix(tempPosition) == 0)
                if(tempPosition == length(outputMatrix))
                    break;
                elseif (zeroCounter > 6)
                    break;
                end
                tempPosition = tempPosition + 1;
                zeroCounter = zeroCounter + 1;
            end
        end
        oneCounter = 0;
        if (zeroCounter <= 6) & (zeroCounter > 0)
            while (outputMatrix(tempPosition) == 1)
                if(tempPosition == length(outputMatrix))
                    break;
                end
                tempPosition = tempPosition + 1;
                oneCounter = oneCounter + 1;
            end
        end
        endPosition = tempPosition;
        if (oneCounter >= 10)
            outputMatrix(position:endPosition) = 1;
            i = endPosition;
        end
    end
    i = i + 1;
end

end