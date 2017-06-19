﻿// Project:         Daggerfall Tools For Unity
// Copyright:       Copyright (C) 2009-2016 Daggerfall Workshop
// Web Site:        http://www.dfworkshop.net
// License:         MIT License (http://www.opensource.org/licenses/mit-license.php)
// Source Code:     https://github.com/Interkarma/daggerfall-unity
// Original Author: Gavin Clayton (interkarma@dfworkshop.net)
// Contributors:    
// 
// Notes:
//

using System;
using System.Text;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using DaggerfallConnect.Arena2;
using DaggerfallConnect.Utility;

namespace DaggerfallConnect.Save
{
    /// <summary>
    /// Represents a SAVEVARS.DAT file (must be a .175 or later save).
    /// </summary>
    public class SaveVars
    {
        #region Fields

        const string filename = "SAVEVARS.DAT";
        const int emperorSonNameOffset = 0x7C;
        const int travelFlagsOffset = 0xF5;
        const int isDayOffset = 0x391;
        const int inDungeonWaterOffset = 0x3A6;
        const int weaponDrawnOffset = 0x3BF;
        const int gameTimeOffset = 0x3C9;
        const int usingLeftHandWeaponOffset = 0x3D9;
        const int cheatFlagsOffset = 0x173B;
        const int lastSkillCheckTimeOffset = 0x179A;
        const int factionDataOffset = 0x17D0;
        const int factionDataLength = 92;

        string emperorSonName = ""; // Randomly chosen and can be used in character history, where it fills in %imp.
        bool recklessTravel = false;
        bool campOutTravel = false;
        bool shipTravel = false;
        bool isDay = false;
        bool inDungeonWater = false;
        bool weaponDrawn = false;
        uint gameTime = 0;
        bool usingLeftHandWeapon = false;
        bool allMapLocationsRevealedMode = false;
        bool godMode = false;
        uint lastSkillCheckTime = 0;

        // Private fields
        FileProxy saveVarsFile = new FileProxy();
        List<FactionFile.FactionData> factions = new List<FactionFile.FactionData>();

        #endregion

        #region Structures and Enumerations

        /// <summary>
        /// Emperor's son's name.
        /// </summary>
        string[] emperorSonNames = { "Pelagius", "Cephorus", "Uriel", "Cassynder", "Voragiel", "Trabbatus" };

        /// <summary>
        /// Travel flags.
        /// </summary>
        [Flags]
        public enum TravelFlags
        {
            None = 0x00, // Cautiously, Foot/Horse, Inns
            Recklessly = 0x01,
            Ship = 0x02,
            CampOut = 0x0A,
        }

        /// <summary>
        /// Weapon status flags.
        /// </summary>
        [Flags]
        public enum WeaponStatusFlags
        {
            WeaponDrawn = 0x40,
        }

        /// <summary>
        /// Cheat flags.
        /// </summary>
        [Flags]
        public enum CheatFlags
        {
            AllMapLocationsRevealedMode = 0x08,
            GodMode = 0x40,
        }

        #endregion

        #region Properties

        public static string Filename
        {
            get { return filename; }
        }

        /// <summary>
        /// Gets Emperor's son's name from savevars.
        /// </summary>
        public string EmperorSonName
        {
            get { return emperorSonName; }
        }

        /// <summary>
        /// Gets whether reckless travel is set from savevars.
        /// </summary>
        public bool RecklessTravel
        {
            get { return recklessTravel; }
        }

        /// <summary>
        /// Gets whether camp out travel is set from savevars.
        /// </summary>
        public bool CampOutTravel
        {
            get { return campOutTravel; }
        }

        /// <summary>
        /// Gets whether ship travel is set from savevars.
        /// </summary>
        public bool ShipTravel
        {
            get { return shipTravel; }
        }

        /// <summary>
        /// Gets whether it is daytime from savevars.
        /// </summary>
        public bool IsDay
        {
            get { return isDay; }
        }

        /// <summary>
        /// Gets whether character is in dungeon water from savevars.
        /// </summary>
        public bool InDungeonWater
        {
            get { return inDungeonWater; }
        }

        /// <summary>
        /// Gets whether weapon is drawn from savevars.
        /// </summary>
        public bool WeaponDrawn
        {
            get { return weaponDrawn; }
        }

        /// <summary>
        /// Gets game time read from savevars.
        /// </summary>
        public uint GameTime
        {
            get { return gameTime; }
        }

        /// <summary>
        /// Gets whether left-hand weapon is being used from savevars.
        /// </summary>
        public bool UsingLeftHandWeapon
        {
            get { return usingLeftHandWeapon; }
        }

        /// <summary>
        /// Gets whether invulnerability cheat is on from savevars.
        /// </summary>
        public bool GodMode
        {
            get { return godMode; }
        }

        /// <summary>
        /// Gets whether cheat to reveal all map locations is on from savevars.
        /// </summary>
        public bool AllMapLocationsRevealedMode
        {
            get { return allMapLocationsRevealedMode; }
        }

        /// <summary>
        /// Gets time of last check for raising skills, read from savevars.
        /// </summary>
        public uint LastSkillCheckTime
        {
            get { return lastSkillCheckTime; }
        }

        /// <summary>
        /// Gets array of factions read from savevars.
        /// </summary>
        public FactionFile.FactionData[] Factions
        {
            get { return factions.ToArray(); }
        }

        #endregion

        #region Constructors

        /// <summary>
        /// Default constructor.
        /// </summary>
        public SaveVars()
        {
        }

        /// <summary>
        /// Open constructor.
        /// </summary>
        /// <param name="saveVarsPath">Full path to SAVEVARS.DAT file.</param>
        /// <param name="readOnly">Flag to open file in read-only mode.</param>
        public SaveVars(string saveVarsPath, bool readOnly = true)
            : base()
        {
            Open(saveVarsPath, readOnly);
        }

        #endregion

        #region Public Methods

        /// <summary>
        /// Opens a SAVEVARS.DAT file.
        /// Always uses FileUsage.UseDisk so possible to write back to original file.
        /// </summary>
        /// <param name="saveVarsPath">Full path to SAVEVARS.DAT file.</param>
        /// <param name="readOnly">Flag to open file in read-only mode.</param>
        public bool Open(string saveVarsPath, bool readOnly = true)
        {
            // Open file proxy
            if (!saveVarsFile.Load(saveVarsPath, FileUsage.UseDisk, readOnly))
                return false;

            // Get reader
            BinaryReader reader = saveVarsFile.GetReader();

            // Read data
            ReadEmperorSonName(reader);
            ReadTravelFlags(reader);
            ReadIsDay(reader);
            ReadInDungeonWater(reader);
            ReadWeaponDrawn(reader);
            ReadGameTime(reader);
            ReadUsingLeftHandWeapon(reader);
            ReadCheatFlags(reader);
            ReadLastSkillCheckTime(reader);
            ReadFactionData(reader);

            return true;
        }

        #endregion

        #region Private Methods

        void ReadEmperorSonName(BinaryReader reader)
        {
            reader.BaseStream.Position = emperorSonNameOffset;
            emperorSonName = emperorSonNames[reader.ReadByte()];
        }

        void ReadTravelFlags(BinaryReader reader)
        {
            reader.BaseStream.Position = travelFlagsOffset;
            short flags = reader.ReadInt16();
            flags = (short)(flags - 533);
            if ((flags & (byte)TravelFlags.Recklessly) != 0)
                recklessTravel = true;
            if ((flags & (byte)TravelFlags.Ship) != 0)
                shipTravel = true;
            if ((flags & (byte)TravelFlags.CampOut) != 0)
                campOutTravel = true;
        }

        void ReadIsDay(BinaryReader reader)
        {
            reader.BaseStream.Position = isDayOffset;
            if (reader.ReadByte() == 1)
                isDay = true;
        }

        void ReadInDungeonWater(BinaryReader reader)
        {
            reader.BaseStream.Position = inDungeonWaterOffset;
            if (reader.ReadByte() == 1)
                inDungeonWater = true;
        }

        void ReadWeaponDrawn(BinaryReader reader)
        {
            reader.BaseStream.Position = weaponDrawnOffset;
            byte flags = reader.ReadByte();
            if ((flags & (byte)WeaponStatusFlags.WeaponDrawn) != 0)
                weaponDrawn = true;
        }

        void ReadGameTime(BinaryReader reader)
        {
            reader.BaseStream.Position = gameTimeOffset;
            gameTime = reader.ReadUInt32();
        }

        void ReadUsingLeftHandWeapon(BinaryReader reader)
        {
            reader.BaseStream.Position = usingLeftHandWeaponOffset;
            if (reader.ReadByte() == 1)
                usingLeftHandWeapon = true;
        }

        void ReadCheatFlags(BinaryReader reader)
        {
            reader.BaseStream.Position = cheatFlagsOffset;
            byte flags = reader.ReadByte();
            if ((flags & (byte)CheatFlags.AllMapLocationsRevealedMode) != 0)
                allMapLocationsRevealedMode = true;
            if ((flags & (byte)CheatFlags.GodMode) != 0)
                godMode = true;
        }

        void ReadLastSkillCheckTime(BinaryReader reader)
        {
            reader.BaseStream.Position = lastSkillCheckTimeOffset;
            lastSkillCheckTime = reader.ReadUInt32();
        }

        void ReadFactionData(BinaryReader reader)
        {
            // Step through factions
            factions.Clear();
            int factionCount = (int)(reader.BaseStream.Length - factionDataOffset) / factionDataLength;
            for (int i = 0; i < factionCount; i++)
            {
                FactionFile.FactionData faction = new FactionFile.FactionData();
                reader.BaseStream.Position = factionDataOffset + (i * factionDataLength);

                faction.type = reader.ReadByte();
                faction.region = reader.ReadSByte();
                faction.ruler = reader.ReadSByte();
                faction.name = FileProxy.ReadCString(reader, 26);

                faction.rep = reader.ReadInt16();
                faction.power = reader.ReadInt16();
                faction.id = reader.ReadInt16();
                faction.vam = reader.ReadInt16();
                faction.flags = reader.ReadInt16();

                reader.BaseStream.Position += 8;            // Skip 8 unknown bytes

                faction.flat1 = reader.ReadInt16();
                faction.flat2 = reader.ReadInt16();

                faction.face = reader.ReadSByte();
                reader.BaseStream.Position += 1;            // Second face index is always -1

                faction.race = reader.ReadSByte();
                faction.sgroup = reader.ReadSByte();
                faction.ggroup = reader.ReadSByte();

                faction.ally1 = reader.ReadInt32();
                faction.ally2 = reader.ReadInt32();
                faction.ally3 = reader.ReadInt32();

                faction.enemy1 = reader.ReadInt32();
                faction.enemy2 = reader.ReadInt32();
                faction.enemy3 = reader.ReadInt32();

                reader.BaseStream.Position += 12;           // Skip 12 unknown bytes

                factions.Add(faction);
            }
        }

        #endregion
    }
}