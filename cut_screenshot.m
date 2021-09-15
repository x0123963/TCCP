% This script includes a user-defined function crop_save
% to process the screenshots for the TCCP stimuli
% Lisa Chang 2021.9.15

stimuli_folder = "C:\Users\user\Documents\TCCP\stimuli\raw\";
% create an output subfolder of the final stimuli
output_folder = stimuli_folder + "item\";
mkdir(output_folder);

% practice items: p1, p2, ..., p12
for p = 1:12
    crop_save(stimuli_folder, "p"+num2str(p));
end
% real experiment items: r1, r2, ..., r250
for r = 1:250
    crop_save(stimuli_folder, "r"+num2str(r));
end

% The following function finds the border of the purple outline,
% cuts the figure to a smaller size, and saves it as a png file
% in a subfolder named "item"
% input arguments: 1) fpath - the directory of the screenshots)
% 2) itemID - filename of the screenshots

function crop_save(fpath,itemID)
    raw = imread(fpath+itemID+".png");
    [row,col] = find(raw(:,:,1)==211&raw(:,:,2)==204&raw(:,:,3)==255);
    upper_edge = min(row);
    right_edge = max(col);
    cut = raw(upper_edge+1:upper_edge+600,right_edge+1:right_edge+350,:);
    imwrite(cut, fpath+"item\"+itemID+".png");
end
