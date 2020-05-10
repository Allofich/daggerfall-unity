// Project:         Daggerfall Tools For Unity
// Copyright:       Copyright (C) 2009-2020 Daggerfall Workshop
// Web Site:        http://www.dfworkshop.net
// License:         MIT License (http://www.opensource.org/licenses/mit-license.php)
// Source Code:     https://github.com/Interkarma/daggerfall-unity
// Original Author: Gavin Clayton (interkarma@dfworkshop.net)
// Contributors:    Allofich
// 
// Notes:
//

using UnityEngine;
using DaggerfallWorkshop.Game.Entity;
using DaggerfallWorkshop.Game.MagicAndEffects;
using System.Collections.Generic;
using DaggerfallWorkshop.Utility;

namespace DaggerfallWorkshop.Game
{
    /// <summary>
    /// Enemy motor and AI combat decision-making logic.
    /// </summary>
    [RequireComponent(typeof(EnemySenses))]
    [RequireComponent(typeof(EnemyAttack))]
    [RequireComponent(typeof(EnemyBlood))]
    [RequireComponent(typeof(EnemySounds))]
    [RequireComponent(typeof(CharacterController))]
    public class EnemyMotor : MonoBehaviour
    {

        #region Member Variables

        public float OpenDoorDistance = 2f;         // Maximum distance to open door
        const float attackSpeedDivisor = 2f;        // How much to slow down during attack animations
        float stopDistance = 1.7f;                  // Used to prevent orbiting
        bool flies;                                 // The enemy can fly
        bool swims;                                 // The enemy can swim
        bool pausePursuit;                          // pause to wait for the player to come closer to ground
        float moveInForAttackTimer;                 // Time until next pursue/retreat decision
        bool moveInForAttack;                       // False = retreat. True = pursue.
        float retreatDistanceMultiplier;            // How far to back off while retreating
        float changeStateTimer;                     // Time until next change in behavior. Padding to prevent instant reflexes.
        bool doStrafe;
        float strafeTimer;
        bool pursuing;                              // Is pursuing
        bool retreating;                            // Is retreating
        bool backingUp;                             // Is backing up
        bool fallDetected;                          // Detected a fall in front of us, so don't move there
        bool obstacleDetected;
        bool foundUpwardSlope;
        bool foundDoor;
        Vector3 lastPosition;                       // Used to track whether we have moved or not
        Vector3 lastDirection;                      // Used to track whether we have rotated or not
        bool rotating;                              // Used to track whether we have rotated or not
        float avoidObstaclesTimer;
        bool checkingClockwise;
        float checkingClockwiseTimer;
        bool didClockwiseCheck;
        float lastTimeWasStuck;
        bool hasBowAttack;
        float realHeight;
        float centerChange;
        bool resetHeight;
        float heightChangeTimer;
        bool strafeLeft;
        float strafeAngle;
        int ignoreMaskForShooting;
        int ignoreMaskForPathing;
        float pathingTimer;
        bool needToPath;
        readonly List<Vector3> pathToTarget = new List<Vector3>();
        readonly List<Vector3> omittedPoints = new List<Vector3>();
        readonly Vector3[] pathChoices = { new Vector3(0, 0, 1), new Vector3(1, 0, 1), new Vector3(1, 0, 0), new Vector3(1, 0, -1), new Vector3(0, 0, -1),
                                            new Vector3(-1, 0, -1), new Vector3(-1, 0, 0), new Vector3(-1, 0, 1)};
        bool canAct;
        bool falls;
        bool flyerFalls;
        float lastGroundedY;                        // Used for fall damage
        float originalHeight;

        EnemySenses senses;
        Vector3 destination;
        Vector3 detourDestination;
        CharacterController controller;
        DaggerfallMobileUnit mobile;
        DaggerfallEntityBehaviour entityBehaviour;
        EnemyBlood entityBlood;
        EntityEffectManager entityEffectManager;
        EntityEffectBundle selectedSpell;
        EnemyAttack attack;
        EnemyEntity entity;
        #endregion

        #region Auto Properties

        public bool IsLevitating { get; set; }      // Is this enemy levitating
        public bool IsHostile { get; set; }         // Is this enemy hostile to the player
        public float KnockbackSpeed { get; set; }   // While non-zero, this enemy will be knocked back at this speed
        public Vector3 KnockbackDirection { get; set; } // Direction to travel while being knocked back
        public bool Bashing { get; private set; }   // Is this enemy bashing a door
        public int GiveUpTimer { get; set; }        // Timer for enemy giving up pursuit of target
        #endregion

        #region Unity Methods

        void Start()
        {
            senses = GetComponent<EnemySenses>();
            controller = GetComponent<CharacterController>();
            mobile = GetComponentInChildren<DaggerfallMobileUnit>();
            IsHostile = mobile.Summary.Enemy.Reactions == MobileReactions.Hostile;
            flies = mobile.Summary.Enemy.Behaviour == MobileBehaviour.Flying ||
                    mobile.Summary.Enemy.Behaviour == MobileBehaviour.Spectral;
            swims = mobile.Summary.Enemy.Behaviour == MobileBehaviour.Aquatic;
            entityBehaviour = GetComponent<DaggerfallEntityBehaviour>();
            entityBlood = GetComponent<EnemyBlood>();
            entityEffectManager = GetComponent<EntityEffectManager>();
            entity = entityBehaviour.Entity as EnemyEntity;
            attack = GetComponent<EnemyAttack>();

            // Only need to check for ability to shoot bow once.
            hasBowAttack = mobile.Summary.Enemy.HasRangedAttack1 && mobile.Summary.Enemy.ID > 129 && mobile.Summary.Enemy.ID != 132;

            // Add things AI should ignore when checking for a clear path to shoot.
            ignoreMaskForShooting = ~(1 << LayerMask.NameToLayer("SpellMissiles") | 1 << LayerMask.NameToLayer("Ignore Raycast"));

            // Set things to ignore when pathing
            ignoreMaskForPathing = ignoreMaskForShooting;
            if (mobile.Summary.Enemy.CanOpenDoors)
                ignoreMaskForPathing |= 1 << LayerMask.NameToLayer("Doors");

            if (!DaggerfallUnity.Settings.EnhancedCombatAI)
                needToPath = false;

            lastGroundedY = transform.position.y;

            // Get original height, before any height adjustments
            originalHeight = controller.height;
        }

        void FixedUpdate()
        {
            if (GameManager.Instance.DisableAI)
                return;

            canAct = true;
            flyerFalls = false;
            falls = false;

            HandleParalysis();
            KnockbackMovement();
            ApplyGravity();
            HandleNoAction();
            HandleBashing();
            UpdateTimers();
            if (canAct)
                TakeAction();
            ApplyFallDamage();
            UpdateToIdleOrMoveAnim();
            OpenDoors();
            HeightAdjust();
        }
        #endregion

        #region Public Methods

        /// <summary>
        /// Immediately become hostile towards attacker and know attacker's location.
        /// </summary>
        /// <param name="attacker">Attacker to become hostile towards</param>
        public void MakeEnemyHostileToAttacker(DaggerfallEntityBehaviour attacker)
        {
            if (!senses)
                senses = GetComponent<EnemySenses>();
            if (!entityBehaviour)
                entityBehaviour = GetComponent<DaggerfallEntityBehaviour>();

            // Assign target if don't already have target, or original target isn't seen or adjacent
            if (attacker && senses && (senses.Target == null || !senses.TargetInSight || senses.DistanceToTarget > 2f))
            {
                senses.Target = attacker;
                senses.SecondaryTarget = senses.Target;
                senses.OldLastKnownTargetPos = attacker.transform.position;
                senses.LastKnownTargetPos = attacker.transform.position;
                senses.PredictedTargetPos = attacker.transform.position;
                GiveUpTimer = 200;
            }

            if (attacker == GameManager.Instance.PlayerEntityBehaviour)
            {
                IsHostile = true;
                // Reset former ally's team
                if (entityBehaviour.Entity.Team == MobileTeams.PlayerAlly)
                {
                    int id = (entityBehaviour.Entity as EnemyEntity).MobileEnemy.ID;
                    entityBehaviour.Entity.Team = EnemyBasics.Enemies[id].Team;
                }
            }
        }

        /// <summary>
        /// Attempts to find the ground position below enemy, even if player is flying/falling
        /// </summary>
        /// <param name="distance">Distance to fire ray.</param>
        /// <returns>Hit point on surface below enemy, or enemy position if hit not found in distance.</returns>
        public Vector3 FindGroundPosition(float distance = 16)
        {
            RaycastHit hit;
            Ray ray = new Ray(transform.position, Vector3.down);
            if (Physics.Raycast(ray, out hit, distance))
                return hit.point;

            return transform.position;
        }
        #endregion

        #region Private Methods

        /// <summary>
        /// Handle paralysis halting movement and animation.
        /// </summary>
        void HandleParalysis()
        {
            // Cancel movement and animations if paralyzed, but still allow gravity to take effect
            // This will have the (intentional for now) side-effect of making paralyzed flying enemies fall out of the air
            // Paralyzed swimming enemies will just freeze in place
            // Freezing anims also prevents the attack from triggering until paralysis cleared
            if (entityBehaviour.Entity.IsParalyzed)
            {
                mobile.FreezeAnims = true;
                canAct = false;
                flyerFalls = true;
            }
            mobile.FreezeAnims = false;
        }

        /// <summary>
        /// Handles movement if the enemy has been knocked back.
        /// </summary>
        void KnockbackMovement()
        {
            // If hit, get knocked back
            if (KnockbackSpeed > 0)
            {
                // Limit KnockbackSpeed. This can be higher than what is actually used for the speed of motion,
                // making it last longer and do more damage if the enemy collides with something (TODO).
                if (KnockbackSpeed > (40 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10)))
                    KnockbackSpeed = (40 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10));

                if (KnockbackSpeed > (5 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10)) &&
                    mobile.Summary.EnemyState != MobileStates.PrimaryAttack)
                {
                    mobile.ChangeEnemyState(MobileStates.Hurt);
                }

                // Actual speed of motion is limited
                Vector3 motion;
                if (KnockbackSpeed <= (25 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10)))
                    motion = KnockbackDirection * KnockbackSpeed;
                else
                    motion = KnockbackDirection * (25 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10));

                // Move in direction of knockback
                if (swims)
                    WaterMove(motion);
                else if (flies || IsLevitating)
                    controller.Move(motion * Time.deltaTime);
                else
                    controller.SimpleMove(motion);

                // Remove remaining knockback and restore animation
                if (GameManager.ClassicUpdate)
                {
                    KnockbackSpeed -= (5 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10));
                    if (KnockbackSpeed <= (5 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10))
                        && mobile.Summary.EnemyState != MobileStates.PrimaryAttack)
                    {
                        mobile.ChangeEnemyState(MobileStates.Move);
                    }
                }

                // If a decent hit got in, reconsider whether to continue current tactic
                if (KnockbackSpeed > (10 / (PlayerSpeedChanger.classicToUnitySpeedUnitRatio / 10)))
                {
                    EvaluateMoveInForAttack();
                }

                canAct = false;
                flyerFalls = true;
            }
        }

        /// <summary>
        /// Apply gravity to ground-based enemies and paralyzed flyers.
        /// </summary>
        void ApplyGravity()
        {
            // Apply gravity
            if (!flies && !swims && !IsLevitating && !controller.isGrounded)
            {
                controller.SimpleMove(Vector3.zero);
                falls = true;

                // Only cancel movement if actually falling. Sometimes mobiles can get stuck where they are !isGrounded but SimpleMove(Vector3.zero) doesn't help.
                // Allowing them to continue and attempt a Move() frees them, but we don't want to allow that if we can avoid it so they aren't moving
                // while falling, which can also accelerate the fall due to anti-bounce downward movement in Move().
                if (lastPosition != transform.position)
                    canAct = false;
            }

            if (flyerFalls && flies && !IsLevitating)
            {
                controller.SimpleMove(Vector3.zero);
                falls = true;
            }
        }

        /// <summary>
        /// Do nothing if no target or after giving up finding the target or if target position hasn't been acquired yet.
        /// </summary>
        void HandleNoAction()
        {
            if (senses.Target == null || GiveUpTimer <= 0 || senses.PredictedTargetPos == EnemySenses.ResetPlayerPos)
            {
                SetChangeStateTimer();

                canAct = false;
            }
        }

        /// <summary>
        /// Handle bashing doors.
        /// </summary>
        void HandleBashing()
        {
            if (Bashing)
            {
                int speed = entity.Stats.LiveSpeed;
                if (GameManager.ClassicUpdate && DFRandom.rand() % speed >= (speed >> 3) + 6 && attack.MeleeTimer == 0)
                {
                    mobile.ChangeEnemyState(MobileStates.PrimaryAttack);
                    attack.ResetMeleeTimer();
                }

                canAct = false;
            }
        }

        /// <summary>
        /// Updates timers used in this class.
        /// </summary>
        void UpdateTimers()
        {
            if (moveInForAttackTimer > 0)
                moveInForAttackTimer -= Time.deltaTime;

            if (avoidObstaclesTimer > 0)
                avoidObstaclesTimer -= Time.deltaTime;

            // Set avoidObstaclesTimer to 0 if got close enough to detourDestination. Only bother checking if possible to move.
            if (avoidObstaclesTimer > 0 && canAct)
            {
                Vector3 detourDestination2D = detourDestination;
                detourDestination2D.y = transform.position.y;
                if ((detourDestination2D - transform.position).magnitude <= 0.3f)
                {
                    avoidObstaclesTimer = 0;
                }
            }

            if (checkingClockwiseTimer > 0)
                checkingClockwiseTimer -= Time.deltaTime;

            if (changeStateTimer > 0)
                changeStateTimer -= Time.deltaTime;

            if (strafeTimer > 0)
                strafeTimer -= Time.deltaTime;

            if (pathingTimer > 0)
                pathingTimer -= Time.deltaTime;

            // As long as the target is detected,
            // giveUpTimer is reset to full
            if (senses.DetectedTarget)
                GiveUpTimer = 200;

            // GiveUpTimer value is from classic, so decrease at the speed of classic's update loop
            if (GameManager.ClassicUpdate && !senses.DetectedTarget && GiveUpTimer > 0)
                GiveUpTimer--;
        }

        /// <summary>
        /// Make decision about what action to take.
        /// </summary>
        void TakeAction()
        {
            // Monster speed of movement follows the same formula as for when the player walks
            float moveSpeed = (entity.Stats.LiveSpeed + PlayerSpeedChanger.dfWalkBase) * MeshReader.GlobalScale;

            // Get isPlayingOneShot for use below
            bool isPlayingOneShot = mobile.IsPlayingOneShot();

            // Reduced speed if playing a one-shot animation with enhanced AI
            if (isPlayingOneShot && DaggerfallUnity.Settings.EnhancedCombatAI)
                moveSpeed /= attackSpeedDivisor;

            // Classic AI moves only as close as melee range. It uses a different range for the player and for other AI.
            if (!DaggerfallUnity.Settings.EnhancedCombatAI)
            {
                if (senses.Target == GameManager.Instance.PlayerEntityBehaviour)
                    stopDistance = attack.MeleeDistance;
                else
                    stopDistance = attack.ClassicMeleeDistanceVsAI;
            }

            // If we can simply move directly to the target, clear any existing path
            if (DaggerfallUnity.Settings.EnhancedCombatAI)
            {
                needToPath = !ClearPathBetweenPositions(transform.position + controller.center, senses.PredictedTargetPos, (senses.PredictedTargetPos - (transform.position + controller.center)).magnitude);

                if (!needToPath)
                    pathToTarget.Clear();

                // If we're following a path and got close to the next point, remove it.
                if (pathToTarget.Count > 0)
                {
                    Vector3 nextPoint = pathToTarget[0];
                    nextPoint.y = (transform.position + controller.center).y;
                    if ((nextPoint - transform.position + controller.center).magnitude <= 1f) // 0.3f
                    {
                        pathToTarget.RemoveAt(0);
                        pathingTimer = 5f;
                    }
                }
                else
                    pathingTimer = 0;

                // If we're following a path and we can shortcut to the next one, omit the current destination. Don't omit the last point, though.
                if (pathToTarget.Count > 2 && Mathf.Abs(pathToTarget[1].y - (transform.position + controller.center).y) <= 0.5f && ClearPathBetweenPositions(transform.position + controller.center, pathToTarget[1], (pathToTarget[1] - (transform.position + controller.center)).magnitude))
                {
                    pathToTarget.RemoveAt(0);
                    pathingTimer = 10f;
                }
            }

            // Get location to move towards.
            GetDestination();

            // Get direction & distance to destination.
            Vector3 direction = (destination - transform.position).normalized;

            float distance;
            // If enemy sees the target, use the distance value from EnemySenses, as this is also used for the melee attack decision and we need to be consistent with that.
            if (avoidObstaclesTimer <= 0 && senses.TargetInSight && pathToTarget.Count == 0)
                distance = senses.DistanceToTarget;
            else
                distance = (destination - transform.position).magnitude;

            // Ranged attacks
            if (DoRangedAttack(direction, moveSpeed, distance, isPlayingOneShot))
                return;

            // Touch spells
            if (DoTouchSpell())
                return;

            // Update advance/retreat decision
            if (moveInForAttackTimer <= 0 && avoidObstaclesTimer <= 0)
                EvaluateMoveInForAttack();

            // If detouring or have a built path, attempt to move
            if (avoidObstaclesTimer > 0 || pathToTarget.Count > 0)
            {
                AttemptMove(direction, moveSpeed);
            }
            // Otherwise, if not still executing a retreat, approach target until close enough to be on-guard.
            // If decided to move in for attack, continue until within melee range. Classic always moves in for attack.
            else if ((!retreating && distance >= (stopDistance * 2.75)) || (distance > stopDistance && moveInForAttack))
            {
                // If state change timer is done, or we are continuing an already started pursuit, we can move immediately
                if (changeStateTimer <= 0 || pursuing)
                    AttemptMove(direction, moveSpeed);
                // Otherwise, look at target until timer finishes
                else if (!senses.TargetIsWithinYawAngle(22.5f, destination))
                    TurnToTarget(direction);
            }
            else if (DaggerfallUnity.Settings.EnhancedCombatAI && strafeTimer <= 0)
            {
                StrafeDecision();
            }
            else if (doStrafe && strafeTimer > 0 && (distance >= stopDistance * .8f))
            {
                AttemptMove(direction, moveSpeed / 4, false, true, distance);
            }
            // Back away from combat target if right next to it, or if decided to retreat and enemy is too close.
            // Classic AI never backs away.
            else if (DaggerfallUnity.Settings.EnhancedCombatAI && senses.TargetInSight && (distance < stopDistance * .8f ||
                !moveInForAttack && distance < stopDistance * retreatDistanceMultiplier && (changeStateTimer <= 0 || retreating)))
            {
                // If state change timer is done, or we are already executing a retreat, we can move immediately
                if (changeStateTimer <= 0 || retreating)
                    AttemptMove(direction, moveSpeed / 2, true);
            }
            // Not moving, just look at target
            else if (!senses.TargetIsWithinYawAngle(22.5f, destination))
            {
                TurnToTarget(direction);
            }
            else // Not moving, and no need to turn
            {
                SetChangeStateTimer();
                pursuing = false;
                retreating = false;
            }
        }

        /// <summary>
        /// Get the destination to move towards.
        /// </summary>
        void GetDestination()
        {
            CharacterController targetController = senses.Target.GetComponent<CharacterController>();
            // If detouring around an obstacle or fall, use the detour position
            if (avoidObstaclesTimer > 0)
            {
                destination = detourDestination;
            }
            // If we are already pathing to the target, use the next path point
            else if (pathingTimer > 0 && pathToTarget.Count > 0)
            {
                destination = pathToTarget[0];
                Vector3 rayDir = pathToTarget[0] - transform.position;
                Debug.DrawRay(transform.position, rayDir, Color.green);
                for (int i = 0; i < pathToTarget.Count - 1; i++)
                {
                    rayDir = pathToTarget[i + 1] - pathToTarget[i];
                    Debug.DrawRay(pathToTarget[i], rayDir, Color.blue);
                }
            }
            // Otherwise, create a path if necessary
            else if (needToPath && pathingTimer <= 0)
            {
                destination = senses.PredictedTargetPos;
                CreatePath();
            }
            // Otherwise, we don't need to path. Just go straight at target.
            else
            {
                destination = senses.PredictedTargetPos;
                // Flying enemies and slaughterfish aim for target face
                if (flies || IsLevitating || (swims && mobile.Summary.Enemy.ID == (int)MonsterCareers.Slaughterfish))
                    destination.y += 0.9f;
            }

            if (avoidObstaclesTimer <= 0 && !flies && !IsLevitating && !swims && senses.Target)
            {
                // Ground enemies target at their own height
                // Otherwise, short enemies' vector can aim up towards the target, which could interfere with distance-to-target calculations.
                float deltaHeight = (targetController.height - originalHeight) / 2;
                destination.y -= deltaHeight;
            }
        }

        /// <summary>
        /// Handles ranged attacks with bows and spells.
        /// </summary>
        bool DoRangedAttack(Vector3 direction, float moveSpeed, float distance, bool isPlayingOneShot)
        {
            bool inRange = senses.DistanceToTarget > EnemyAttack.minRangedDistance && senses.DistanceToTarget < EnemyAttack.maxRangedDistance;
            if (inRange && senses.TargetInSight && senses.DetectedTarget && (CanShootBow() || CanCastRangedSpell()))
            {
                if (DaggerfallUnity.Settings.EnhancedCombatAI && senses.TargetIsWithinYawAngle(22.5f, destination) && strafeTimer <= 0)
                {
                    StrafeDecision();
                }

                if (doStrafe && strafeTimer > 0)
                {
                    AttemptMove(direction, moveSpeed / 4, false, true, distance);
                }

                if (GameManager.ClassicUpdate && senses.TargetIsWithinYawAngle(22.5f, destination))
                {
                    if (!isPlayingOneShot)
                    {
                        if (hasBowAttack)
                        {
                            // Random chance to shoot bow
                            if (Random.value < 1 / 32f)
                            {
                                if (mobile.Summary.Enemy.HasRangedAttack1 && !mobile.Summary.Enemy.HasRangedAttack2)
                                    mobile.ChangeEnemyState(MobileStates.RangedAttack1);
                                else if (mobile.Summary.Enemy.HasRangedAttack2)
                                    mobile.ChangeEnemyState(MobileStates.RangedAttack2);
                            }
                        }
                        // Random chance to shoot spell
                        else if (Random.value < 1 / 40f && entityEffectManager.SetReadySpell(selectedSpell))
                        {
                            mobile.ChangeEnemyState(MobileStates.Spell);
                        }
                    }
                }
                else
                    TurnToTarget(direction);

                return true;
            }

            return false;
        }

        /// <summary>
        /// Handles touch-range spells.
        /// </summary>
        bool DoTouchSpell()
        {
            if (senses.TargetInSight && senses.DetectedTarget && attack.MeleeTimer == 0
                && senses.DistanceToTarget <= attack.MeleeDistance + senses.TargetRateOfApproach
                && CanCastTouchSpell() && entityEffectManager.SetReadySpell(selectedSpell))
            {
                if (mobile.Summary.EnemyState != MobileStates.Spell)
                    mobile.ChangeEnemyState(MobileStates.Spell);

                attack.ResetMeleeTimer();
                return true;
            }

            return false;
        }

        /// <summary>
        /// Decide whether to strafe, and get direction to strafe to.
        /// </summary>
        void StrafeDecision()
        {
            doStrafe = Random.Range(0, 4) == 0;
            strafeTimer = Random.Range(1f, 2f);
            if (doStrafe)
            {
                if (Random.Range(0, 2) == 0)
                    strafeLeft = true;
                else
                    strafeLeft = false;

                Vector3 north = destination;
                north.z++; // Adding 1 to z so this Vector3 will be north of the destination Vector3.

                // Get angle between vector from destination to the north of it, and vector from destination to this enemy's position
                strafeAngle = Vector3.SignedAngle(destination - north, destination - transform.position, Vector3.up);
                if (strafeAngle < 0)
                    strafeAngle = 360 + strafeAngle;

                // Convert to radians
                strafeAngle *= Mathf.PI / 180;
            }
        }

        /// <summary>
        /// Returns whether there is a clear path to move the given distance from location1 to location2. True if no obstacle hit
        /// or if combat target is the first obstacle hit.
        /// </summary>
        bool ClearPathBetweenPositions(Vector3 position1, Vector3 position2, float dist)
        {
            if (!flies && !swims && !IsLevitating && Mathf.Abs(position2.y - position1.y) > 0.5f)
                return false;

            Vector3 sphereCastDir = (position2 - position1).normalized;
            Vector3 sphereCastDir2d = sphereCastDir;
            sphereCastDir2d.y = 0;

            RaycastHit hit;
            if (Physics.SphereCast(transform.position, controller.radius / 2, sphereCastDir, out hit, dist, ignoreMaskForPathing))
            {
                if (hit.transform.GetComponent<DaggerfallEntityBehaviour>() == senses.Target)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                // Check for a difference in elevation and for a fall along the way to the target
                if (!flies && !swims && !IsLevitating)
                {
                    Vector3 rayOrigin = position1 + (position2 - position1) / 2;
                    Ray ray = new Ray(rayOrigin, Vector3.down);
                    bool acceptableDrop = Physics.Raycast(ray, 2.5f);

                    if (!acceptableDrop)
                        return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Returns true if can shoot projectile at target.
        /// </summary>
        bool HasClearPathToShootProjectile(float speed, float radius)
        {
            // Check that there is a clear path to shoot projectile
            Vector3 sphereCastDir = senses.PredictNextTargetPos(speed);
            if (sphereCastDir == EnemySenses.ResetPlayerPos)
                return false;

            float sphereCastDist = (sphereCastDir - transform.position).magnitude;
            sphereCastDir = (sphereCastDir - transform.position).normalized;

            RaycastHit hit;
            if (Physics.SphereCast(transform.position, radius, sphereCastDir, out hit, sphereCastDist, ignoreMaskForShooting))
            {
                DaggerfallEntityBehaviour hitTarget = hit.transform.GetComponent<DaggerfallEntityBehaviour>();

                // Clear path to target
                if (hitTarget == senses.Target)
                    return true;

                // Something in the way
                return false;
            }

            // Clear path to predicted target position
            return true;
        }

        /// <summary>
        /// Returns true if can shoot bow at target.
        /// </summary>
        bool CanShootBow()
        {
            if (!hasBowAttack)
                return false;

            // Check that there is a clear path to shoot an arrow
            // All arrows are currently 35 speed.
            return HasClearPathToShootProjectile(35f, 0.15f);
        }

        /// <summary>
        /// Selects a ranged spell from this enemy's list and returns true if it can be cast.
        /// </summary>
        bool CanCastRangedSpell()
        {
            if (entity.CurrentMagicka <= 0)
                return false;

            EffectBundleSettings[] spells = entity.GetSpells();
            List<EffectBundleSettings> rangeSpells = new List<EffectBundleSettings>();
            int count = 0;
            foreach (EffectBundleSettings spell in spells)
            {
                if (spell.TargetType == TargetTypes.SingleTargetAtRange
                    || spell.TargetType == TargetTypes.AreaAtRange)
                {
                    rangeSpells.Add(spell);
                    count++;
                }
            }

            if (count == 0)
                return false;

            EffectBundleSettings selectedSpellSettings = rangeSpells[Random.Range(0, count)];
            selectedSpell = new EntityEffectBundle(selectedSpellSettings, entityBehaviour);

            if (EffectsAlreadyOnTarget(selectedSpell))
                return false;

            // Check that there is a clear path to shoot a spell
            // All range spells are currently 25 speed and 0.45f radius
            return HasClearPathToShootProjectile(25f, 0.45f);
        }

        /// <summary>
        /// Selects a touch spell from this enemy's list and returns true if it can be cast.
        /// </summary>
        bool CanCastTouchSpell()
        {
            if (entity.CurrentMagicka <= 0)
                return false;

            EffectBundleSettings[] spells = entity.GetSpells();
            List<EffectBundleSettings> rangeSpells = new List<EffectBundleSettings>();
            int count = 0;
            foreach (EffectBundleSettings spell in spells)
            {
                // Classic AI considers ByTouch and CasterOnly here
                if (!DaggerfallUnity.Settings.EnhancedCombatAI)
                {
                    if (spell.TargetType == TargetTypes.ByTouch
                        || spell.TargetType == TargetTypes.CasterOnly)
                    {
                        rangeSpells.Add(spell);
                        count++;
                    }
                }
                else // Enhanced AI considers ByTouch and AreaAroundCaster. TODO: CasterOnly logic
                {
                    if (spell.TargetType == TargetTypes.ByTouch
                        || spell.TargetType == TargetTypes.AreaAroundCaster)
                    {
                        rangeSpells.Add(spell);
                        count++;
                    }
                }
            }

            if (count == 0)
                return false;

            EffectBundleSettings selectedSpellSettings = rangeSpells[Random.Range(0, count)];
            selectedSpell = new EntityEffectBundle(selectedSpellSettings, entityBehaviour);

            if (EffectsAlreadyOnTarget(selectedSpell))
                return false;

            return true;
        }

        /// <summary>
        /// Checks whether the target already is affected by all of the effects of the given spell.
        /// </summary>
        bool EffectsAlreadyOnTarget(EntityEffectBundle spell)
        {
            if (senses.Target)
            {
                EntityEffectManager targetEffectManager = senses.Target.GetComponent<EntityEffectManager>();
                LiveEffectBundle[] bundles = targetEffectManager.EffectBundles;

                for (int i = 0; i < spell.Settings.Effects.Length; i++)
                {
                    bool foundEffect = false;
                    // Get effect template
                    IEntityEffect effectTemplate = GameManager.Instance.EntityEffectBroker.GetEffectTemplate(spell.Settings.Effects[i].Key);
                    for (int j = 0; j < bundles.Length && !foundEffect; j++)
                    {
                        for (int k = 0; k < bundles[j].liveEffects.Count && !foundEffect; k++)
                        {
                            if (bundles[j].liveEffects[k].GetType() == effectTemplate.GetType())
                                foundEffect = true;
                        }
                    }

                    if (!foundEffect)
                        return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Try to move in given direction.
        /// </summary>
        void AttemptMove(Vector3 direction, float moveSpeed, bool backAway = false, bool strafe = false, float strafeDist = 0)
        {
            // Set whether pursuing or retreating, for bypassing changeStateTimer delay when continuing these actions
            if (!backAway && !strafe)
            {
                pursuing = true;
                retreating = false;
            }
            else
            {
                retreating = true;
                pursuing = false;
            }

            if (!senses.TargetIsWithinYawAngle(5.625f, destination))
            {
                TurnToTarget(direction);
                // Classic always turns in place. Enhanced only does so if enemy is not in sight,
                // for more natural-looking movement while pursuing.
                if (!DaggerfallUnity.Settings.EnhancedCombatAI || !senses.TargetInSight)
                    return;
            }

            if (backAway)
                direction *= -1;

            if (strafe)
            {
                Vector3 strafeDest = new Vector3(destination.x + (Mathf.Sin(strafeAngle) * strafeDist), transform.position.y, destination.z + (Mathf.Cos(strafeAngle) * strafeDist));
                direction = (strafeDest - transform.position).normalized;

                if ((strafeDest - transform.position).magnitude <= 0.2f)
                {
                    if (strafeLeft)
                        strafeAngle++;
                    else
                        strafeAngle--;
                }
            }

            // Move downward some to eliminate bouncing down inclines
            if (!flies && !swims && !IsLevitating && controller.isGrounded)
                direction.y = -2f;

            // Stop fliers from moving too near the floor during combat
            if (flies && avoidObstaclesTimer <= 0 && direction.y < 0 && FindGroundPosition((originalHeight / 2) + 1f) != transform.position)
                direction.y = 0.1f;

            Vector3 motion = direction * moveSpeed;

            // If using enhanced combat, avoid moving directly below targets
            if (!backAway && DaggerfallUnity.Settings.EnhancedCombatAI && avoidObstaclesTimer == 0 && pathToTarget.Count == 0)
            {
                bool withinPitch = senses.TargetIsWithinPitchAngle(45.0f);
                if (!pausePursuit && !withinPitch)
                {
                    if (flies || IsLevitating || swims)
                    {
                        if (!senses.TargetIsAbove())
                            motion = -transform.up * moveSpeed / 2;
                        else
                            motion = transform.up * moveSpeed;
                    }
                    // Causes a random delay after being out of pitch range
                    else if (senses.TargetIsAbove() && changeStateTimer <= 0)
                    {
                        SetChangeStateTimer();
                        pausePursuit = true;
                    }
                }
                else if (withinPitch)
                {
                    pausePursuit = false;
                    backingUp = false;
                }

                if (pausePursuit)
                {
                    if (senses.TargetIsAbove() && !senses.TargetIsWithinPitchAngle(55.0f) && (changeStateTimer <= 0 || backingUp))
                    {
                        // Back away from target
                        motion = -transform.forward * moveSpeed * 0.75f;
                        backingUp = true;
                    }
                    else
                    {
                        // Stop moving
                        backingUp = false;
                        return;
                    }
                }
            }

            SetChangeStateTimer();

            // Check if there is something to collide with directly in movement direction, such as upward sloping ground.
            Vector3 direction2d = direction;
            if (!flies && !swims && !IsLevitating)
                direction2d.y = 0;
            ObstacleCheck(direction2d);
            FallCheck(direction2d);

            if (fallDetected || obstacleDetected)
            {
                if (!strafe && !backAway)
                    FindDetour(direction2d);
            }
            else
            // Clear to move
            {
                if (swims)
                    WaterMove(motion);
                else
                    controller.Move(motion * Time.deltaTime);
            }
        }

        /// <summary>
        /// Try to find a way around an obstacle or fall.
        /// </summary>
        void FindDetour(Vector3 direction2d)
        {
            float angle;
            Vector3 testMove = Vector3.zero;
            bool foundUpDown = false;

            // Try up/down first
            if (flies || swims || IsLevitating)
            {
                float multiplier = 0.3f;
                if (Random.Range(0, 2) == 0)
                    multiplier = -0.3f;

                Vector3 upOrDown = new Vector3(0, 1, 0);
                upOrDown.y *= multiplier;

                testMove = (direction2d + upOrDown).normalized;

                ObstacleCheck(testMove);
                if (obstacleDetected)
                {
                    upOrDown.y *= -1;
                    testMove = (direction2d + upOrDown).normalized;
                    ObstacleCheck(testMove);
                }
                if (!obstacleDetected)
                    foundUpDown = true;
            }

            // Reset clockwise check if we've been clear of obstacles/falls for a while
            if (!foundUpDown && Time.time - lastTimeWasStuck > 2f)
            {
                checkingClockwiseTimer = 0;
                didClockwiseCheck = false;
            }

            if (!foundUpDown && checkingClockwiseTimer <= 0)
            {
                if (!didClockwiseCheck)
                {
                    // Check 45 degrees in both ways first
                    // Pick first direction to check randomly
                    if (Random.Range(0, 2) == 0)
                        angle = 45;
                    else
                        angle = -45;

                    testMove = Quaternion.AngleAxis(angle, Vector3.up) * direction2d;
                    ObstacleCheck(testMove);
                    FallCheck(testMove);

                    if (!obstacleDetected && !fallDetected)
                    {
                        // First direction was clear, use that way
                        if (angle == 45)
                        {
                            checkingClockwise = true;
                        }
                        else
                            checkingClockwise = false;
                    }
                    else
                    {
                        // Tested 45 degrees in the clockwise/counter-clockwise direction we chose,
                        // but hit something, so try other one.
                        angle *= -1;
                        testMove = Quaternion.AngleAxis(angle, Vector3.up) * direction2d;
                        ObstacleCheck(testMove);
                        FallCheck(testMove);

                        if (!obstacleDetected && !fallDetected)
                        {
                            if (angle == 45)
                            {
                                checkingClockwise = true;
                            }
                            else
                                checkingClockwise = false;
                        }
                        else
                        {
                            // Both 45 degrees checks failed, pick clockwise/counterclockwise based on angle to target
                            Vector3 toTarget = destination - transform.position;
                            Vector3 directionToTarget = toTarget.normalized;
                            angle = Vector3.SignedAngle(directionToTarget, direction2d, Vector3.up);

                            if (angle > 0)
                            {
                                checkingClockwise = true;
                            }
                            else
                                checkingClockwise = false;
                        }
                    }
                    checkingClockwiseTimer = 5;
                    didClockwiseCheck = true;
                }
                else
                {
                    didClockwiseCheck = false;
                    checkingClockwise = !checkingClockwise;
                    checkingClockwiseTimer = 5;
                }
            }

            angle = 0;
            int count = 0;

            if (!foundUpDown)
            {
                do
                {
                    if (checkingClockwise)
                        angle += 45;
                    else
                        angle -= 45;

                    testMove = Quaternion.AngleAxis(angle, Vector3.up) * direction2d;
                    ObstacleCheck(testMove);
                    FallCheck(testMove);

                    // Break out of loop if can't find anywhere to go
                    count++;
                    if (count > 7)
                    {
                        break;
                    }
                }
                while (obstacleDetected || fallDetected);
            }

            detourDestination = transform.position + testMove * 2;

            if (avoidObstaclesTimer <= 0)
                avoidObstaclesTimer = 0.75f;
            lastTimeWasStuck = Time.time;
        }

        void ObstacleCheck(Vector3 direction)
        {
            obstacleDetected = false;
            const float checkDistance = 0.8f;
            foundUpwardSlope = false;
            foundDoor = false;

            RaycastHit hit;
            // Climbable/not climbable step for the player seems to be at around a height of 0.65f. The player is 1.8f tall.
            // Using the same ratio to height as these values, set the capsule for the enemy. 
            Vector3 p1 = transform.position + (Vector3.up * -originalHeight * 0.1388F);
            Vector3 p2 = p1 + (Vector3.up * Mathf.Min(originalHeight, 1.75f) / 2);

            if (Physics.CapsuleCast(p1, p2, controller.radius / 2, direction, out hit, checkDistance))
            {
                // Debug.DrawRay(transform.position, direction, Color.red, 2.0f);
                obstacleDetected = true;
                DaggerfallEntityBehaviour entityBehaviour2 = hit.transform.GetComponent<DaggerfallEntityBehaviour>();
                DaggerfallActionDoor door = hit.transform.GetComponent<DaggerfallActionDoor>();
                DaggerfallLoot loot = hit.transform.GetComponent<DaggerfallLoot>();

                if (entityBehaviour2)
                {
                    if (entityBehaviour2 == senses.Target)
                        obstacleDetected = false;
                }
                else if (door)
                {
                    obstacleDetected = false;
                    foundDoor = true;
                    if (senses.TargetIsWithinYawAngle(22.5f, door.transform.position))
                    {
                        senses.LastKnownDoor = door;
                        senses.DistanceToDoor = Vector3.Distance(transform.position, door.transform.position);
                    }
                }
                else if (loot)
                {
                    obstacleDetected = false;
                }
                else if (!swims && !flies && !IsLevitating)
                {
                    // If an obstacle was hit, check for a climbable upward slope
                    Vector3 checkUp = transform.position + direction;
                    checkUp.y++;

                    direction = (checkUp - transform.position).normalized;
                    p1 = transform.position + (Vector3.up * -originalHeight * 0.25f);
                    p2 = p1 + (Vector3.up * originalHeight * 0.75f);

                    if (!Physics.CapsuleCast(p1, p2, controller.radius / 2, direction, checkDistance))
                    {
                        obstacleDetected = false;
                        foundUpwardSlope = true;
                    }
                }
            }
            else
            {
                // Debug.DrawRay(transform.position, direction, Color.green, 2.0f);
            }
        }

        void FallCheck(Vector3 direction)
        {
            if (flies || IsLevitating || swims || obstacleDetected || foundUpwardSlope || foundDoor)
            {
                fallDetected = false;
                return;
            }

            int checkDistance = 1;
            Vector3 rayOrigin = transform.position;

            direction *= checkDistance;
            Ray ray = new Ray(rayOrigin + direction, Vector3.down);
            RaycastHit hit;

            fallDetected = !Physics.Raycast(ray, out hit, (originalHeight * 0.5f) + 1.5f);
        }

        /// <summary>
        /// Decide whether or not to pursue enemy, based on perceived combat odds.
        /// </summary>
        void EvaluateMoveInForAttack()
        {
            // Classic always attacks
            if (!DaggerfallUnity.Settings.EnhancedCombatAI)
            {
                moveInForAttack = true;
                return;
            }

            // No retreat from unseen opponent
            if (!senses.TargetInSight)
            {
                moveInForAttack = true;
                return;
            }

            // No retreat if enemy is paralyzed
            if (senses.Target != null)
            {
                EntityEffectManager targetEffectManager = senses.Target.GetComponent<EntityEffectManager>();
                if (targetEffectManager.FindIncumbentEffect<MagicAndEffects.MagicEffects.Paralyze>() != null)
                {
                    moveInForAttack = true;
                    return;
                }

                // No retreat if enemy's back is turned
                if (senses.TargetHasBackTurned())
                {
                    moveInForAttack = true;
                    return;
                }

                // No retreat if enemy is player with bow or weapon not out
                if (senses.Target == GameManager.Instance.PlayerEntityBehaviour
                    && GameManager.Instance.WeaponManager.ScreenWeapon
                    && (GameManager.Instance.WeaponManager.ScreenWeapon.WeaponType == WeaponTypes.Bow
                    || !GameManager.Instance.WeaponManager.ScreenWeapon.ShowWeapon))
                {
                    moveInForAttack = true;
                    return;
                }
            }
            else
            {
                return;
            }

            const float retreatDistanceBaseMult = 2.25f;

            // Level difference affects likelihood of backing away.
            moveInForAttackTimer = Random.Range(1, 3);
            int levelMod = (entity.Level - senses.Target.Entity.Level) / 2;
            if (levelMod > 4)
                levelMod = 4;
            if (levelMod < -4)
                levelMod = -4;

            int roll = Random.Range(0 + levelMod, 10 + levelMod);

            moveInForAttack = roll > 4;

            // Chose to retreat
            if (!moveInForAttack)
            {
                retreatDistanceMultiplier = (float)(retreatDistanceBaseMult + (retreatDistanceBaseMult * (0.25 * (2 - roll))));

                if (!DaggerfallUnity.Settings.EnhancedCombatAI)
                    return;

                if (Random.Range(0, 2) == 0)
                    strafeLeft = true;
                else
                    strafeLeft = false;

                Vector3 north = destination;
                north.z++; // Adding 1 to z so this Vector3 will be north of the destination Vector3.

                // Get angle between vector from destination to the north of it, and vector from destination to this enemy's position
                strafeAngle = Vector3.SignedAngle(destination - north, destination - transform.position, Vector3.up);
                if (strafeAngle < 0)
                    strafeAngle = 360 + strafeAngle;

                // Convert to radians
                strafeAngle *= Mathf.PI / 180;
            }
        }

        /// <summary>
        /// Set timer for padding between state changes, for non-perfect reflexes.
        /// </summary>
        void SetChangeStateTimer()
        {
            // No timer without enhanced AI
            if (!DaggerfallUnity.Settings.EnhancedCombatAI)
                return;

            if (changeStateTimer <= 0)
                changeStateTimer = Random.Range(0.2f, .8f);
        }

        /// <summary>
        /// Movement for water enemies.
        /// </summary>
        void WaterMove(Vector3 motion)
        {
            // Don't allow aquatic enemies to go above the water level of a dungeon block
            if (GameManager.Instance.PlayerEnterExit.blockWaterLevel != 10000
                    && controller.transform.position.y
                    < GameManager.Instance.PlayerEnterExit.blockWaterLevel * -1 * MeshReader.GlobalScale)
            {
                if (motion.y > 0 && controller.transform.position.y + (100 * MeshReader.GlobalScale)
                        >= GameManager.Instance.PlayerEnterExit.blockWaterLevel * -1 * MeshReader.GlobalScale)
                {
                    motion.y = 0;
                }
                controller.Move(motion * Time.deltaTime);
            }
        }

        /// <summary>
        /// Rotate toward target.
        /// </summary>
        void TurnToTarget(Vector3 targetDirection)
        {
            const float turnSpeed = 20f;
            //Classic speed is 11.25f, too slow for Daggerfall Unity's agile player movement

            if (GameManager.ClassicUpdate)
            {
                transform.forward = Vector3.RotateTowards(transform.forward, targetDirection, turnSpeed * Mathf.Deg2Rad, 0.0f);
            }
        }

        /// <summary>
        /// Set to either idle or move animation depending on whether the enemy has moved or rotated.
        /// </summary>
        void UpdateToIdleOrMoveAnim()
        {
            if (!mobile.IsPlayingOneShot())
            {
                // Rotation is done at classic update rate, so check at classic update rate
                if (GameManager.ClassicUpdate)
                {
                    Vector3 currentDirection = transform.forward;
                    currentDirection.y = 0;
                    rotating = lastDirection != currentDirection;
                    lastDirection = currentDirection;
                }
                // Movement is done at regular update rate, so check position at regular update rate
                if (!rotating && lastPosition == transform.position)
                    mobile.ChangeEnemyState(MobileStates.Idle);
                else
                    mobile.ChangeEnemyState(MobileStates.Move);
            }

            lastPosition = transform.position;
        }

        void ApplyFallDamage()
        {
            // Assuming the same formula is used for the player and enemies
            const float fallingDamageThreshold = 5.0f;
            const float HPPerMetre = 5f;

            if (controller.isGrounded)
            {
                // did enemy just land?
                if (falls)
                {
                    float fallDistance = lastGroundedY - transform.position.y;
                    if (fallDistance > fallingDamageThreshold)
                    {
                        int damage = (int)(HPPerMetre * (fallDistance - fallingDamageThreshold));

                        EnemyEntity enemyEntity = entityBehaviour.Entity as EnemyEntity;
                        enemyEntity.DecreaseHealth(damage);

                        if (entityBlood)
                        {
                            // Like in classic, falling enemies bleed at the center. It must hurt the center of mass ;)
                            entityBlood.ShowBloodSplash(0, transform.position);
                        }

                        DaggerfallUI.Instance.DaggerfallAudioSource.PlayClipAtPoint((int)SoundClips.FallDamage, FindGroundPosition());
                    }
                }

                lastGroundedY = transform.position.y;
            }
        }

        /// <summary>
        /// Open doors that are in the way.
        /// </summary>
        void OpenDoors()
        {
            // Try to open doors blocking way
            if (mobile.Summary.Enemy.CanOpenDoors)
            {
                if (senses.LastKnownDoor != null
                    && senses.DistanceToDoor < OpenDoorDistance && !senses.LastKnownDoor.IsOpen
                    && !senses.LastKnownDoor.IsLocked)
                {
                    senses.LastKnownDoor.ToggleDoor();
                    return;
                }

                // If door didn't open, and we are trying to get to the target, bash
                Bashing = DaggerfallUnity.Settings.EnhancedCombatAI && !senses.TargetInSight && moveInForAttack
                    && senses.LastKnownDoor != null && senses.DistanceToDoor <= attack.MeleeDistance && senses.LastKnownDoor.IsLocked;
            }
        }

        /// <summary>
        /// Create a path to the destination.
        /// </summary>
        void CreatePath()
        {
            const int searchLimit = 70; // How many times can the search iterate without finding a solution

            pathToTarget.Clear();
            // Set up the start point to path from
            Vector3 rayOrigin = transform.position + controller.center;

            // Round to the nearest 1x1 point and start searching from it, if it's not blocked
            Vector3 roundedOrigin = new Vector3(Mathf.Round(rayOrigin.x), rayOrigin.y, Mathf.Round(rayOrigin.z));
            Vector3 p1 = transform.position + controller.center + (Vector3.up * -controller.height * 0.25F);
            Vector3 p2 = p1 + (Vector3.up * controller.height / 2);
            if (!Physics.CapsuleCast(p1, p2, controller.radius, (roundedOrigin - rayOrigin).normalized, (roundedOrigin - rayOrigin).magnitude, ignoreMaskForPathing))
                rayOrigin = roundedOrigin;

            // Limit how many omitted points we remember
            if (omittedPoints.Count > 300)
                omittedPoints.Clear();

            // Choose whether to search right or left first, based on direction to target compared to direction we're facing now.
            // If it's to the left (signed angle is negative), we may have been searching to the right and probably should continue doing so.
            Vector3 toDest2D = destination - rayOrigin;
            toDest2D.y = 0;
            int searchDirectionMultiplier = Vector3.SignedAngle(transform.forward, toDest2D, Vector3.up) < 0 ? 1 : -1;

            // Remember which points we have used as ray origins or destinations to eliminate searching the same points multiple times
            List<Vector3> usedOrigins = new List<Vector3> { rayOrigin };
            List<Vector3> usedDestinations = new List<Vector3> { rayOrigin };
            List<int> directionCounts = new List<int> { 0 };

            // For the very first direction to check use the direction we're already facing
            // as it is likely to be the correct one.
            Vector3 north = transform.forward;
            north.z++; // Adding 1 to z so this Vector3 will be north of the transform.forward vector.
                       // Get direction angle from this enemy's position to transform.forward vector
            float angle = Vector3.SignedAngle(north - transform.forward, transform.forward - rayOrigin, Vector3.up);

            // Make sure angle is positive
            if (angle < 0)
                angle = 360 + angle;

            // Get the index of the 8-directional position to check that matches the angle
            int index = (int)((angle + (45 / 2)) / 45);
            if (index > 7 || index < 0)
                index = 0;
            List<int> indexes = new List<int> { index };

            Vector3 rayDir;
            Vector3 rayDest;

            // Variables for determining if should bother looking up or down.
            bool canGoUp = true;
            bool canGoDown = true;

            // Prevent land-based enemies from searching up or down if the target is in the other direction
            if (!flies && !swims && !IsLevitating)
            {
                if ((destination.y - rayOrigin.y) > 1.5f)
                    canGoDown = false;
                else if ((rayOrigin.y - destination.y) > 1.5f)
                    canGoUp = false;
            }

            // Variables for determining if the search was successful or target/destination was seen
            bool success = false;
            bool sawDestination = false;

            // Iterative search. The head of the list of origins is the one currently being searched with.
            int count = 0;

            // Get starting distance if target visible
            //float startDistance = 0f;
            RaycastHit hit;
            //if (Physics.SphereCast(rayOrigin, 0.45f, (destination - rayOrigin).normalized, out hit, (destination - rayOrigin).magnitude, ignoreMaskForShooting))
            //{
            //    if (hit.transform.GetComponent<DaggerfallEntityBehaviour>() == senses.Target)
            //        sawDestination = true;
            //}
            //else
            //    sawDestination = true;

            //if (sawDestination)
           //     startDistance = (destination - rayOrigin).magnitude;

            // Reset sawDestination for use below
            //sawDestination = false;

            while (count <= searchLimit && usedOrigins.Count > 0)
            {
                rayOrigin = usedOrigins[0];

                if (count != 0 && directionCounts[0] == 0) // If checking the first of the 8 directions
                {
                    north = destination;
                    north.z++; // Adding 1 to z so this Vector3 will be north of the destination Vector3.
                               // Get direction angle from this enemy's position to destination
                    angle = Vector3.SignedAngle(north - destination, destination - rayOrigin, Vector3.up);

                    // Make sure angle is positive
                    if (angle < 0)
                        angle = 360 + angle;

                    // Get the 8-directional position to check that matches the angle
                    index = (int)((angle + (45 / 2)) / 45);
                    if (index > 7 || index < 0)
                        index = 0;
                    indexes.Insert(0, index);
                }

                // Get the 8-directional position to check
                rayDest = rayOrigin + pathChoices[indexes[0]];

                // Non-ground based enemies can freely path up and down
                if (flies || swims || IsLevitating)
                {
                    Ray ray1 = new Ray(rayOrigin, Vector3.up);
                    Ray ray2 = new Ray(rayOrigin, Vector3.down);
                    if ((destination.y - rayOrigin.y) > 1 && !Physics.SphereCast(ray1, controller.radius, 1, ignoreMaskForPathing))
                        rayDest.y++;
                    else if ((destination.y - rayOrigin.y) < -1 && !Physics.SphereCast(ray2, controller.radius, 1, ignoreMaskForPathing))
                        rayDest.y--;
                }

                // Check that the position to check hasn't been checked or omitted. If it has, get the next one.
                while ((omittedPoints.Contains(rayDest) || usedDestinations.Contains(rayDest)) && directionCounts[0] <= 7)
                {
                    directionCounts[0]++;
                    indexes[0] += searchDirectionMultiplier * directionCounts[0];
                    if (indexes[0] > 7)
                        indexes[0] -= 8;
                    if (indexes[0] < 0)
                        indexes[0] += 8;
                    rayDest = rayOrigin + pathChoices[indexes[0]];
                }

                // Get the direction to the position to check
                rayDir = rayDest - rayOrigin;

                // Advance counters to next direction for the next time this point is checked
                directionCounts[0]++;
                indexes[0] += searchDirectionMultiplier;
                if (indexes[0] > 7)
                    indexes[0] -= 8;
                if (indexes[0] < 0)
                    indexes[0] += 8;

                // Remove the front of the lists if all 8 directions have been checked for this ray origin
                if (directionCounts[0] > 7)
                {
                    omittedPoints.Add(usedOrigins[0]); // Might be a useless place to check, so omit from future searches for a while
                    directionCounts.RemoveAt(0);
                    indexes.RemoveAt(0);
                    usedOrigins.RemoveAt(0);
                }

                // Check for an obstacle on the way to this point
                bool omitted = false;
                p1 = rayOrigin + (Vector3.up * -controller.height * 0.15f);
                p2 = p1 + (Vector3.up * controller.height * 0.30f);
                if (!Physics.CapsuleCast(p1, p2, controller.radius, rayDir, out hit, (rayDest - rayOrigin).magnitude, ignoreMaskForPathing))
                {
                    // No obstacle found. For ground enemies, check that any drop here is minor
                    bool acceptableDrop = true;
                    if (!flies && !swims && !IsLevitating)
                    {
                        float acceptableDropDist = 2.5f;
                        //if (!canGoDown)
                        //    acceptableDropDist = controller.height;

                        acceptableDrop = Physics.Raycast(rayDest, Vector3.down, out hit, acceptableDropDist, ignoreMaskForPathing);

                        if (acceptableDrop)
                        {
                            rayDest.y = Mathf.Min(rayDest.y - hit.distance + (controller.height / 2), rayDest.y);
                        }
                    }

                    if (acceptableDrop)
                    {
                        usedOrigins.Insert(0, rayDest);
                        directionCounts.Insert(0, 0);
                        usedDestinations.Add(rayDest);
                    }
                    // Omit falls from future searches for a while
                    else
                    {
                        omitted = true;
                        omittedPoints.Add(rayDest);
                    }
                }
                else // Hit an obstacle
                {
                    // If the combat target was hit, we don't need to search anymore
                    DaggerfallEntityBehaviour hitTarget = hit.transform.GetComponent<DaggerfallEntityBehaviour>();
                    if (hitTarget == senses.Target)
                    {
                        break;
                    }

                    if (canGoUp)
                    {
                        // Handle slopes
                        bool foundSlope = false;

                        /*Vector3 rayDestUp = rayDest;
                        rayDestUp.y += 0.5f;
                        if (!Physics.CapsuleCast(p1, p2, controller.radius, (rayDestUp - rayOrigin).normalized, out hit, (rayDestUp - rayOrigin).magnitude, ignoreMaskForPathing))
                        {
                            //rayDest.y++;
                            usedOrigins.Insert(0, rayDestUp);
                            usedDestinations.Add(rayDestUp);
                            directionCounts.Insert(0, 0);
                            foundSlope = true;
                        }*/

                        // Set y for low ray to just above bottom of controller
                        Vector3 slopeRayOrigin = rayOrigin;
                        slopeRayOrigin.y -= ((controller.height / 2) - 0.1f);
                        Ray ray = new Ray(slopeRayOrigin, rayDir);
                        RaycastHit lowHit;
                        bool firstRayHit = Physics.Raycast(ray, out lowHit, (rayDest - rayOrigin).magnitude);

                        // Aim a little higher for next ray. Should be enough for the ray to hit the next step on a climbable staircase,
                        // but not so much that a non-climbable difference in height is mistaken as a climbable slope.
                        slopeRayOrigin.y += 0.5f; // 0.3f
                        ray = new Ray(slopeRayOrigin, rayDir);
                        RaycastHit highHit;
                        bool secondRayHit = Physics.Raycast(ray, out highHit, (rayDest - rayOrigin).magnitude);
                        if (firstRayHit && (!secondRayHit || (lowHit.distance < highHit.distance - 0.3f))) // 0.1f
                        {
                            rayDest.y++;
                            usedOrigins.Insert(0, rayDest);
                            usedDestinations.Add(rayDest);
                            directionCounts.Insert(0, 0);
                            foundSlope = true;
                        }

                        // If no slope was found, and the obstacle we hit wasn't another DaggerfallBehaviour, reject the destination for a while as it's probably a wall
                        if (!foundSlope && !hitTarget)
                        {
                            omitted = true;
                            omittedPoints.Add(rayDest);
                        }
                    }
                }

                Debug.DrawRay(rayOrigin, (rayDest - rayOrigin).normalized * (rayDest - rayOrigin).magnitude, Color.red);

                // This rayDest is close to the destination, so stop searching
                if (!omitted && ClearPathBetweenPositions(rayDest, destination, (destination - rayDest).magnitude))
                {
                    success = true;
                    break;
                }
                else
                {
                    // Check if the target/destination is visible from this point
                    if (Physics.SphereCast(rayOrigin, 0.45f, (destination - rayOrigin).normalized, out hit, (destination - rayOrigin).magnitude, ignoreMaskForPathing))
                    {
                        if (hit.transform.GetComponent<DaggerfallEntityBehaviour>() == senses.Target)
                            sawDestination = true;
                    }
                    else
                        sawDestination = true;
                }

                count++;
            }

            // Collect the results if the result is better than where we started
            //bool resultCloser = usedOrigins.Count > 0 && (startDistance == 0 || ((destination - usedOrigins[0]).magnitude < startDistance));

            foreach (Vector3 origin in usedOrigins)
            {
                //if (resultCloser)
                    pathToTarget.Insert(0, origin);

                // If didn't get to destination and destination wasn't seen, we probably need to move on somewhere else.
                // Omit every part of this path for a while.
                if (!success && !sawDestination)
                    omittedPoints.Insert(0, origin);
            }

            pathingTimer = 5f;
        }


        /// <summary>
        /// Limits maximum controller height.
        /// Tall sprites require this hack to get through doors.
        /// </summary>
        void HeightAdjust()
        {
            // If enemy bumps into something, temporarily reduce their height to 1.65, which should be short enough to fit through most if not all doorways.
            // Unfortunately, while the enemy is shortened, projectiles will not collide with the top of the enemy for the difference in height.
            if (!resetHeight && controller && ((controller.collisionFlags & CollisionFlags.CollidedSides) != 0) && originalHeight > 1.65f)
            {
                // Adjust the center of the controller so that sprite doesn't sink into the ground
                centerChange = (1.65f - controller.height) / 2;
                Vector3 newCenter = controller.center;
                newCenter.y += centerChange;
                controller.center = newCenter;
                // Adjust the height
                controller.height = 1.65f;
                resetHeight = true;
                heightChangeTimer = 0.5f;
            }
            else if (resetHeight && heightChangeTimer <= 0)
            {
                // Restore the original center
                Vector3 newCenter = controller.center;
                newCenter.y -= centerChange;
                controller.center = newCenter;
                // Restore the original height
                controller.height = originalHeight;
                resetHeight = false;
            }

            if (resetHeight && heightChangeTimer > 0)
            {
                heightChangeTimer -= Time.deltaTime;
            }
        }
        #endregion
    }
}
