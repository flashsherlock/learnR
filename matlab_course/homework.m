Path='/Volumes/WD_D/gufei/fsl2019/dpabi/demo/FunImgARCWF/';
DataFile_name='Filtered_4Dvolume.nii';
Mask=y_ReadAll('/Volumes/WD_D/gufei/DPABI_V4.3_200401/Templates/BrainMask_05_61x73x61.img');
MeanData=zeros(61,73,61);    
for sub=1:3
	DataFile=[Path 'Sub_00' num2str(sub) '/' DataFile_name];
	[Data,Vox,Files,Header]=y_ReadAll(DataFile);
	datasize=size(Data);
	%data_reshape=reshape(Data,[],230);
	%Seed=mean(data_reshape,1);
	Seed=squeeze(mean(mean(mean(Data,1),2),3));
	OutData=zeros(datasize(1:3));    
    for i=1:61
		for j=1:73
			for k=1:61
				if Mask(i,j,k)
					VoxelT=squeeze(Data(i,j,k,:));
					[r,~]=corrcoef(Seed,VoxelT);
					OutData(i,j,k)=r(1,2);
				end
			end
		end
    end
    MeanData=OutData+MeanData;
	Outname=[Path 'Sub_00' num2str(sub) '_Con' '.nii'];
	y_Write(OutData,Header,Outname);
end
%average
MeanData=MeanData/3;
y_Write(MeanData,Header,'Mean.nii');

%plot
Targetdir='/Volumes/WD_D/gufei/fsl2019/dpabi/demo/TestImages/';
images=dir('*.nii');
for i=1:length(images)
    ImageFile=images(i).name;
   
    H=w_Call_DPABI_VIEW(ImageFile);
    [Image,~]=w_MontageImage([-30 -15 0 15 30],'T',H);
    Image=flip(Image,1);
    imwrite(Image,[Targetdir,'/',ImageFile(1:end-4),'_A5.tif']);
	
    [Image,~]=w_MontageImage([-30 -15 0 15 30],'C',H);
    Image=flip(Image,1);
	imwrite(Image,[Targetdir,'/',ImageFile(1:end-4),'_C5.tif']);
	

    [Image,~]=w_MontageImage([-30 -15 0 15 30],'S',H);
    Image=flip(Image,1);
    imwrite(Image,[Targetdir,'/',ImageFile(1:end-4),'_S5.tif']);
	
	%     [ImageA2,Space2]=w_MontageImage([-70:6:-46;-46:6:-22;-22:6:2;2:6:26;26:6:50],'T',H);
	%     ImageA2=flip(ImageA2,1);
	%     imwrite(ImageA2,[Targetdir,'/',ImageFile(1:end-4),'_A25.tif']);
	
end
close all